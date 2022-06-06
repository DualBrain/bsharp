Imports System.Collections.Immutable
Imports System.Reflection
Imports Basic.CodeAnalysis.Binding
Imports Ccl = Mono.Cecil
Imports Basic.CodeAnalysis.Symbols
Imports Mono.Cecil.Cil
Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports System.Security.Cryptography
Imports Microsoft.VisualBasic.CompilerServices
Imports Basic.CodeAnalysis.Syntax
Imports System.Text

Namespace Global.Basic.CodeAnalysis.Emit

  Friend NotInheritable Class Emitter

    Private ReadOnly _diagnostics As New DiagnosticBag
    Private ReadOnly _knownTypes As New Dictionary(Of TypeSymbol, Ccl.TypeReference)
    Private ReadOnly _objectEqualsReference As MethodReference
    Private ReadOnly _consoleReadLineReference As MethodReference
    Private ReadOnly _consoleWriteLineReference As MethodReference
    Private ReadOnly _stringConcat2Reference As MethodReference
    Private ReadOnly _stringConcat3Reference As MethodReference
    Private ReadOnly _stringConcat4Reference As MethodReference
    Private ReadOnly _stringConcatArrayReference As MethodReference
    Private ReadOnly _convertToBooleanReference As MethodReference
    Private ReadOnly _convertToInt32Reference As MethodReference
    Private ReadOnly _convertToStringReference As MethodReference
    Private ReadOnly _randomReference As TypeReference
    Private ReadOnly _randomCtorReference As MethodReference
    Private ReadOnly _randomNextReference As MethodReference
    Private ReadOnly _assemblyDefinition As AssemblyDefinition
    Private ReadOnly _methods As New Dictionary(Of FunctionSymbol, MethodDefinition)
    Private ReadOnly _locals As New Dictionary(Of VariableSymbol, VariableDefinition)
    Private ReadOnly _labels As New Dictionary(Of BoundLabel, Integer)
    Private ReadOnly _fixups As New List(Of (InstructionIndex As Integer, Target As BoundLabel))

    Private ReadOnly _typeDefinition As TypeDefinition
    Private _randomFieldDefinition As FieldDefinition

    ' TODO: This constructor does too much. Resolution should be factored out.
    Private Sub New(moduleName As String, references() As String)

      Dim assemblies = New List(Of AssemblyDefinition)

      For Each reference In references
        Try
          Dim assembly = Ccl.AssemblyDefinition.ReadAssembly(reference)
          assemblies.Add(assembly)
        Catch ex As BadImageFormatException
          _diagnostics.ReportInvalidReference(reference)
        End Try
      Next

      Dim builtInTypes = New List(Of (typeSymbol As TypeSymbol, metadataName As String)) From {
        (TypeSymbol.Any, "System.Object"),
        (TypeSymbol.Boolean, "System.Boolean"),
        (TypeSymbol.Integer, "System.Int32"),
        (TypeSymbol.String, "System.String"),
        (TypeSymbol.Nothing, "System.Void")
      }

      Dim assemblyName = New AssemblyNameDefinition(moduleName, New Version(1, 0))
      _assemblyDefinition = AssemblyDefinition.CreateAssembly(assemblyName, moduleName, ModuleKind.Console)
      _knownTypes = New Dictionary(Of TypeSymbol, TypeReference)

      For Each entry In builtInTypes
        Dim typeReference = Emit_ResolveType(assemblies, entry.typeSymbol.Name, entry.metadataName)
        _knownTypes.Add(entry.typeSymbol, typeReference)
      Next

      _objectEqualsReference = Emit_ResolveMethod(assemblies, "System.Object", "Equals", {"System.Object", "System.Object"})
      _consoleReadLineReference = Emit_ResolveMethod(assemblies, "System.Console", "ReadLine", Array.Empty(Of String))
      _consoleWriteLineReference = Emit_ResolveMethod(assemblies, "System.Console", "WriteLine", {"System.Object"})
      _stringConcat2Reference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String", "System.String"})
      _stringConcat3Reference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String", "System.String", "System.String"})
      _stringConcat4Reference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String", "System.String", "System.String", "System.String"})
      _stringConcatArrayReference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String[]"})
      _convertToBooleanReference = Emit_ResolveMethod(assemblies, "System.Convert", "ToBoolean", {"System.Object"})
      _convertToInt32Reference = Emit_ResolveMethod(assemblies, "System.Convert", "ToInt32", {"System.Object"})
      _convertToStringReference = Emit_ResolveMethod(assemblies, "System.Convert", "ToString", {"System.Object"})

      _randomReference = Emit_ResolveType(assemblies, Nothing, "System.Random")
      _randomCtorReference = Emit_ResolveMethod(assemblies, "System.Random", ".ctor", Array.Empty(Of String))
      _randomNextReference = Emit_ResolveMethod(assemblies, "System.Random", "Next", {"System.Int32"})

      Dim objectType = _knownTypes(TypeSymbol.Any)
      If objectType IsNot Nothing Then
        _typeDefinition = New TypeDefinition("", "Program", Ccl.TypeAttributes.Abstract Or Ccl.TypeAttributes.Sealed, objectType)
        _assemblyDefinition.MainModule.Types.Add(_typeDefinition)
      Else
        _typeDefinition = Nothing
      End If

    End Sub

    Public Shared Function Emit(program As BoundProgram, moduleName As String, references() As String, outputPath As String) As ImmutableArray(Of Diagnostic)

      If program.ErrorDiagnostics.Any Then Return program.Diagnostics

      Dim emitter = New Emitter(moduleName, references)
      Return emitter.Emit(program, outputPath)

    End Function

    Public Function Emit(program As BoundProgram, outputPath As String) As ImmutableArray(Of Diagnostic)

      If _diagnostics.Any Then Return _diagnostics.ToImmutableArray

      For Each functionWithBody In program.Functions
        EmitFunctionDeclaration(functionWithBody.Key)
      Next

      For Each functionWithBody In program.Functions
        EmitFunctionBody(functionWithBody.Key, functionWithBody.Value)
      Next

      If program.MainFunction IsNot Nothing Then
        _assemblyDefinition.EntryPoint = _methods(program.MainFunction)
      End If

      _assemblyDefinition.Write(outputPath)

      Return _diagnostics.ToImmutableArray

    End Function

    Private Sub EmitFunctionDeclaration(func As FunctionSymbol)
      Dim functionType = _knownTypes(func.Type)
      Dim method = New MethodDefinition(func.Name, Ccl.MethodAttributes.Static Or Ccl.MethodAttributes.Private, functionType)
      For Each parameter In func.Parameters
        Dim parameterType = _knownTypes(parameter.Type)
        Dim parameterAttributes = Ccl.ParameterAttributes.None
        Dim parameterDefinition = New Ccl.ParameterDefinition(parameter.Name, parameterAttributes, parameterType)
        method.Parameters.Add(parameterDefinition)
      Next
      _typeDefinition.Methods.Add(method)
      _methods.Add(func, method)
    End Sub

    Private Sub EmitFunctionBody(func As FunctionSymbol, body As BoundBlockStatement)
      Dim method = _methods(func)
      _locals.Clear()
      _labels.Clear()
      _fixups.Clear()
      Dim ilProcessor = method.Body.GetILProcessor
      For Each statement In body.Statements
        EmitStatement(ilProcessor, statement)
      Next
      For Each fixup In _fixups
        Dim targetLabel = fixup.Target
        Dim targetInstructionIndex = _labels(targetLabel)
        Dim targetInstruction = ilProcessor.Body.Instructions(targetInstructionIndex)
        Dim instructionToFixup = ilProcessor.Body.Instructions(fixup.InstructionIndex)
        instructionToFixup.Operand = targetInstruction
      Next
      method.Body.OptimizeMacros
    End Sub

    Private Sub EmitStatement(ilProcessor As ILProcessor, node As BoundStatement)
      Select Case node.Kind
        Case BoundNodeKind.NopStatement : EmitNopStatement(ilProcessor, CType(node, BoundNopStatement))
        Case BoundNodeKind.VariableDeclaration : EmitVariableDeclaration(ilProcessor, CType(node, BoundVariableDeclaration))
        Case BoundNodeKind.LabelStatement : EmitLabelStatement(ilProcessor, CType(node, BoundLabelStatement))
        Case BoundNodeKind.GotoStatement : EmitGotoStatement(ilProcessor, CType(node, BoundGotoStatement))
        Case BoundNodeKind.ConditionalGotoStatement : EmitConditionalGotoStatement(ilProcessor, CType(node, BoundConditionalGotoStatement))
        Case BoundNodeKind.ReturnStatement : EmitReturnStatement(ilProcessor, CType(node, BoundReturnStatement))
        Case BoundNodeKind.ExpressionStatement : EmitExpressionStatement(ilProcessor, CType(node, BoundExpressionStatement))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    Private Sub EmitNopStatement(ilProcessor As ILProcessor, node As BoundNopStatement)
      If node Is Nothing Then
      End If
      ilProcessor.Emit(OpCodes.Nop)
    End Sub

    Private Sub EmitVariableDeclaration(ilProcessor As ILProcessor, node As BoundVariableDeclaration)
      Dim typeReference = _knownTypes(node.Variable.Type)
      Dim variableDefinition = New VariableDefinition(typeReference)
      _locals.Add(node.Variable, variableDefinition)
      ilProcessor.Body.Variables.Add(variableDefinition)
      EmitExpression(ilProcessor, node.Initializer)
      ilProcessor.Emit(OpCodes.Stloc, variableDefinition)
    End Sub

    Private Sub EmitLabelStatement(ilProcessor As ILProcessor, node As BoundLabelStatement)
      _labels.Add(node.Label, ilProcessor.Body.Instructions.Count)
    End Sub

    Private Sub EmitGotoStatement(ilProcessor As ILProcessor, node As BoundGotoStatement)
      _fixups.Add((ilProcessor.Body.Instructions.Count, node.Label))
      ilProcessor.Emit(OpCodes.Br, Instruction.Create(OpCodes.Nop))
    End Sub

    Private Sub EmitConditionalGotoStatement(ilProcessor As ILProcessor, node As BoundConditionalGotoStatement)
      EmitExpression(ilProcessor, node.Condition)
      Dim opCode = If(node.JumpIfTrue, OpCodes.Brtrue, OpCodes.Brfalse)
      _fixups.Add((ilProcessor.Body.Instructions.Count, node.Label))
      ilProcessor.Emit(opCode, Instruction.Create(OpCodes.Nop))
    End Sub

    Private Sub EmitReturnStatement(ilProcessor As ILProcessor, node As BoundReturnStatement)
      If node.Expression IsNot Nothing Then EmitExpression(ilProcessor, node.Expression)
      ilProcessor.Emit(OpCodes.Ret)
    End Sub

    Private Sub EmitExpressionStatement(ilProcessor As ILProcessor, node As BoundExpressionStatement)
      EmitExpression(ilProcessor, node.Expression)
      If node.Expression.Type IsNot TypeSymbol.Nothing Then
        ilProcessor.Emit(OpCodes.Pop)
      End If
    End Sub

    Private Sub EmitExpression(ilProcessor As ILProcessor, node As BoundExpression)

      If node.ConstantValue IsNot Nothing Then
        EmitConstantExpression(ilProcessor, node)
        Return
      End If

      Select Case node.Kind
        Case BoundNodeKind.VariableExpression : EmitVariableExpression(ilProcessor, CType(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : EmitAssignmentExpression(ilProcessor, CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : EmitUnaryExpression(ilProcessor, CType(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : EmitBinaryExpression(ilProcessor, CType(node, BoundBinaryExpression))
        Case BoundNodeKind.CallExpression : EmitCallExpression(ilProcessor, CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : EmitConversionExpression(ilProcessor, CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    Private Sub EmitConstantExpression(ilProcessor As ILProcessor, node As BoundExpression)

      Debug.Assert(node.ConstantValue IsNot Nothing)

      If node.Type Is TypeSymbol.Boolean Then
        Dim value = CBool(node.ConstantValue.Value)
        Dim instruction = If(value, OpCodes.Ldc_I4_1, OpCodes.Ldc_I4_0)
        ilProcessor.Emit(instruction)
      ElseIf node.Type Is TypeSymbol.Integer Then
        Dim value = CInt(node.ConstantValue.Value)
        ilProcessor.Emit(OpCodes.Ldc_I4, value)
      ElseIf node.Type Is TypeSymbol.String Then
        Dim value = CStr(node.ConstantValue.Value)
        ilProcessor.Emit(OpCodes.Ldstr, value)
      Else
        Throw New Exception($"Unexpected constant expression type: {node.Type}")
      End If
    End Sub

    Private Sub EmitVariableExpression(ilProcessor As ILProcessor, node As BoundVariableExpression)
      If TypeOf node.Variable Is ParameterSymbol Then
        Dim parameter = CType(node.Variable, ParameterSymbol)
        ilProcessor.Emit(OpCodes.Ldarg, parameter.Ordinal)
      Else
        Dim variableDefinition = _locals(node.Variable)
        ilProcessor.Emit(OpCodes.Ldloc, variableDefinition)
      End If
    End Sub

    Private Sub EmitAssignmentExpression(ilProcessor As ILProcessor, node As BoundAssignmentExpression)
      Dim variableDefinition = _locals(node.Variable)
      EmitExpression(ilProcessor, node.Expression)
      ilProcessor.Emit(OpCodes.Dup)
      ilProcessor.Emit(OpCodes.Stloc, variableDefinition)
    End Sub

    Private Sub EmitUnaryExpression(ilProcessor As ILProcessor, node As BoundUnaryExpression)
      EmitExpression(ilProcessor, node.Operand)
      If node.Op.Kind = BoundUnaryOperatorKind.Identity Then
        ' Done.
      ElseIf node.Op.Kind = BoundUnaryOperatorKind.LogicalNegation Then
        ilProcessor.Emit(OpCodes.Ldc_I4_0)
        ilProcessor.Emit(OpCodes.Ceq)
      ElseIf node.Op.Kind = BoundUnaryOperatorKind.Negation Then
        ilProcessor.Emit(OpCodes.Neg)
      ElseIf node.Op.Kind = BoundUnaryOperatorKind.BitwiseComplement Then
        ilProcessor.Emit(OpCodes.Not)
      Else
        Throw New Exception($"Unexpected unary operator {SyntaxFacts.GetText(node.Op.SyntaxKind)}({node.Operand.Type})")
      End If
    End Sub

    Private Sub EmitBinaryExpression(ilProcessor As ILProcessor, node As BoundBinaryExpression)

      ' +(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.Addition Then
        If node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String Then
          EmitStringConcatExpression(ilProcessor, node)
          Return
        End If
      End If

      EmitExpression(ilProcessor, node.Left)
      EmitExpression(ilProcessor, node.Right)

      ' ==(any, any)
      ' ==(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.Equal Then
        If (node.Left.Type Is TypeSymbol.Any AndAlso node.Right.Type Is TypeSymbol.Any) OrElse
           (node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String) Then
          ilProcessor.Emit(OpCodes.Call, _objectEqualsReference)
          Return
        End If
      End If

      ' !=(any, any)
      ' !=(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.NotEqual Then
        If (node.Left.Type Is TypeSymbol.Any AndAlso node.Right.Type Is TypeSymbol.Any) OrElse
           (node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String) Then
          ilProcessor.Emit(OpCodes.Call, _objectEqualsReference)
          ilProcessor.Emit(OpCodes.Ldc_I4_0)
          ilProcessor.Emit(OpCodes.Ceq)
          Return
        End If
      End If

      Select Case node.Op.Kind
        Case BoundBinaryOperatorKind.Addition
          ilProcessor.Emit(OpCodes.Add)
        Case BoundBinaryOperatorKind.Subtraction
          ilProcessor.Emit(OpCodes.Sub)
        Case BoundBinaryOperatorKind.Multiplication
          ilProcessor.Emit(OpCodes.Mul)
        Case BoundBinaryOperatorKind.Division
          ilProcessor.Emit(OpCodes.Div)
        Case BoundBinaryOperatorKind.LogicalAnd
          'TODO: Implement short-circuit evaluation #111
          ilProcessor.Emit(OpCodes.And)
        Case BoundBinaryOperatorKind.LogicalOr
          'TODO: Implement short-circuit evaluation #111
          ilProcessor.Emit(OpCodes.Or)
        Case BoundBinaryOperatorKind.BitwiseAnd
          ilProcessor.Emit(OpCodes.And)
        Case BoundBinaryOperatorKind.BitwiseOr
          ilProcessor.Emit(OpCodes.Or)
        Case BoundBinaryOperatorKind.BitwiseXor
          ilProcessor.Emit(OpCodes.Xor)
        Case BoundBinaryOperatorKind.Equal
          ilProcessor.Emit(OpCodes.Ceq)
        Case BoundBinaryOperatorKind.NotEqual
          ilProcessor.Emit(OpCodes.Ceq)
          ilProcessor.Emit(OpCodes.Ldc_I4_0)
          ilProcessor.Emit(OpCodes.Ceq)
        Case BoundBinaryOperatorKind.LessThan
          ilProcessor.Emit(OpCodes.Clt)
        Case BoundBinaryOperatorKind.LessThanEqual
          ilProcessor.Emit(OpCodes.Cgt)
          ilProcessor.Emit(OpCodes.Ldc_I4_0)
          ilProcessor.Emit(OpCodes.Ceq)
        Case BoundBinaryOperatorKind.GreaterThan
          ilProcessor.Emit(OpCodes.Cgt)
        Case BoundBinaryOperatorKind.GreaterThanEqual
          ilProcessor.Emit(OpCodes.Clt)
          ilProcessor.Emit(OpCodes.Ldc_I4_0)
          ilProcessor.Emit(OpCodes.Ceq)
        Case Else
          Throw New Exception($"Unexpected binary operator {SyntaxFacts.GetText(node.Op.SyntaxKind)}({node.Op.Type})")
      End Select

    End Sub

    Private Sub EmitCallExpression(ilProcessor As ILProcessor, node As BoundCallExpression)

      If node.Function Is BuiltinFunctions.Rnd Then

        If _randomFieldDefinition Is Nothing Then
          EmitRandomField()
        End If

        ilProcessor.Emit(OpCodes.Ldsfld, _randomFieldDefinition)
        For Each argument In node.Arguments
          EmitExpression(ilProcessor, argument)
        Next
        ilProcessor.Emit(OpCodes.Callvirt, _randomNextReference)

        Return

      End If

      For Each argument In node.Arguments
        EmitExpression(ilProcessor, argument)
      Next

      If node.Function Is Input Then
        ilProcessor.Emit(OpCodes.Call, _consoleReadLineReference)
      ElseIf node.Function Is Print Then
        ilProcessor.Emit(OpCodes.Call, _consoleWriteLineReference)
      Else
        Dim methodDefinition = _methods(node.Function)
        ilProcessor.Emit(OpCodes.Call, methodDefinition)
      End If

    End Sub

    Private Sub EmitRandomField()
      _randomFieldDefinition = New FieldDefinition("$rnd", Ccl.FieldAttributes.Static Or
                                                           Ccl.FieldAttributes.Private, _randomReference)
      _typeDefinition.Fields.Add(_randomFieldDefinition)
      Dim staticConstructor = New Ccl.MethodDefinition(".cctor", Ccl.MethodAttributes.Static Or
                                                                 Ccl.MethodAttributes.Private Or
                                                                 Ccl.MethodAttributes.SpecialName Or
                                                                 Ccl.MethodAttributes.RTSpecialName, _knownTypes(TypeSymbol.Nothing))
      _typeDefinition.Methods.Insert(0, staticConstructor)
      Dim ilProcessor = staticConstructor.Body.GetILProcessor
      ilProcessor.Emit(OpCodes.Newobj, _randomCtorReference)
      ilProcessor.Emit(OpCodes.Stsfld, _randomFieldDefinition)
      ilProcessor.Emit(OpCodes.Ret)
    End Sub

    Private Sub EmitConversionExpression(ilProcessor As ILProcessor, node As BoundConversionExpression)
      EmitExpression(ilProcessor, node.Expression)
      Dim needsBoxing = node.Expression.Type Is TypeSymbol.Boolean OrElse
                        node.Expression.Type Is TypeSymbol.Integer
      If needsBoxing Then ilProcessor.Emit(OpCodes.Box, _knownTypes(node.Expression.Type))
      If node.Type Is TypeSymbol.Any Then
        ' Done.
      ElseIf node.Type Is TypeSymbol.Boolean Then
        ilProcessor.Emit(OpCodes.Call, _convertToBooleanReference)
      ElseIf node.Type Is TypeSymbol.Integer Then
        ilProcessor.Emit(OpCodes.Call, _convertToInt32Reference)
      ElseIf node.Type Is TypeSymbol.String Then
        ilProcessor.Emit(OpCodes.Call, _convertToStringReference)
      Else
        Throw New Exception($"Unexpected conversion from {node.Expression.Type} to {node.Type}")
      End If
    End Sub

#Region "Converted from 'inline' functions."

    Private Function Emit_ResolveType(assemblies As List(Of AssemblyDefinition),
                                      internalName As String,
                                      metadataName As String) As TypeReference
      Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
                                  SelectMany(Function(m) m.Types).
                                  Where(Function(t) t.FullName = metadataName).ToArray
      If foundTypes.Length = 1 Then
        Dim typeReference = _assemblyDefinition.MainModule.ImportReference(foundTypes(0))
        Return typeReference
      ElseIf foundTypes.Length = 0 Then
        _diagnostics.ReportRequiredTypeNotFound(internalName, metadataName)
      Else
        _diagnostics.ReportRequiredTypeAmbiguous(internalName, metadataName, foundTypes)
      End If
      Return Nothing
    End Function

    Private Function Emit_ResolveMethod(assemblies As List(Of AssemblyDefinition),
                                        typeName As String,
                                        methodName As String,
                                        parameterTypeNames As String()) As MethodReference
      Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
                                  SelectMany(Function(m) m.Types).
                                  Where(Function(t) t.FullName = typeName).ToArray
      If foundTypes.Length = 1 Then
        Dim foundType = foundTypes(0)
        Dim methods = foundType.Methods.Where(Function(m) m.Name = methodName)

        For Each method In methods
          If method.Parameters.Count <> parameterTypeNames.Length Then
            Continue For
          End If
          Dim allParametersMatch = True
          For i = 0 To parameterTypeNames.Length - 1
            If method.Parameters(i).ParameterType.FullName <> parameterTypeNames(i) Then
              allParametersMatch = False
              Exit For
            End If
          Next
          If Not allParametersMatch Then
            Continue For
          End If
          Return _assemblyDefinition.MainModule.ImportReference(method)
        Next
        _diagnostics.ReportRequiredMethodNotFound(typeName, methodName, parameterTypeNames)
        Return Nothing
      ElseIf foundTypes.Length = 0 Then
        _diagnostics.ReportRequiredTypeNotFound(Nothing, typeName)
      Else
        _diagnostics.ReportRequiredTypeAmbiguous(Nothing, typeName, foundTypes)
      End If
      Return Nothing
    End Function

#End Region

    Private Sub EmitStringConcatExpression(_ilProcessor As ILProcessor, node As BoundBinaryExpression)

      ' Flatten the expression tree to a sequence of nodes to concatenate, then fold consecutive constants in that sequence.
      ' This approach enables constant folding of non-sibling nodes, which cannot be done in the ConstantFolding class as it would require changing the tree.
      ' Example: folding b And c in ((a + b) + c) if they are constant.

      Dim nodes = EmitStringConcatExpression_FoldConstants(EmitStringConcatExpression_Flatten(node)).ToList()

      Select Case nodes.Count
        Case 0
          _ilProcessor.Emit(OpCodes.Ldstr, String.Empty)

        Case 1
          EmitExpression(_ilProcessor, nodes(0))

        Case 2
          EmitExpression(_ilProcessor, nodes(0))
          EmitExpression(_ilProcessor, nodes(1))
          _ilProcessor.Emit(OpCodes.[Call], _stringConcat2Reference)

        Case 3
          EmitExpression(_ilProcessor, nodes(0))
          EmitExpression(_ilProcessor, nodes(1))
          EmitExpression(_ilProcessor, nodes(2))
          _ilProcessor.Emit(OpCodes.[Call], _stringConcat3Reference)

        Case 4
          EmitExpression(_ilProcessor, nodes(0))
          EmitExpression(_ilProcessor, nodes(1))
          EmitExpression(_ilProcessor, nodes(2))
          EmitExpression(_ilProcessor, nodes(3))
          _ilProcessor.Emit(OpCodes.[Call], _stringConcat4Reference)

        Case Else
          _ilProcessor.Emit(OpCodes.Ldc_I4, nodes.Count)
          _ilProcessor.Emit(OpCodes.Newarr, _knownTypes(TypeSymbol.[String]))
          For i = 0 To nodes.Count - 1
            _ilProcessor.Emit(OpCodes.Dup)
            _ilProcessor.Emit(OpCodes.Ldc_I4, i)
            EmitExpression(_ilProcessor, nodes(i))
            _ilProcessor.Emit(OpCodes.Stelem_Ref)
          Next
          _ilProcessor.Emit(OpCodes.[Call], _stringConcatArrayReference)

      End Select

    End Sub

    Private Iterator Function EmitStringConcatExpression_Flatten(node As BoundExpression) As IEnumerable(Of BoundExpression)

      Dim binaryExpression = TryCast(node, BoundBinaryExpression)
      If binaryExpression IsNot Nothing AndAlso
         binaryExpression.Op.Kind = BoundBinaryOperatorKind.Addition AndAlso
         binaryExpression.Left.Type Is TypeSymbol.[String] AndAlso
         binaryExpression.Right.Type Is TypeSymbol.[String] Then

        For Each result In EmitStringConcatExpression_Flatten(binaryExpression.Left)
          Yield result
        Next

        For Each result In EmitStringConcatExpression_Flatten(binaryExpression.Right)
          Yield result
        Next

      Else

        If node.Type IsNot TypeSymbol.[String] Then
          Throw New Exception($"Unexpected node type in string concatenation: {node.Type}")
        End If

        Yield node

      End If

    End Function

    Private Iterator Function EmitStringConcatExpression_FoldConstants(nodes As IEnumerable(Of BoundExpression)) As IEnumerable(Of BoundExpression)

      Dim sb As StringBuilder = Nothing

      For Each node In nodes
        If node.ConstantValue IsNot Nothing Then
          Dim stringValue = CStr(node.ConstantValue.Value)

          If String.IsNullOrEmpty(stringValue) Then
            Continue For
          End If

          sb = If(sb, New StringBuilder)
          sb.Append(stringValue)

        Else

          If sb?.Length > 0 Then
            Yield New BoundLiteralExpression(sb.ToString())
            sb.Clear()
          End If

          Yield node

        End If
      Next

      If sb?.Length > 0 Then
        Yield New BoundLiteralExpression(sb.ToString())
      End If

    End Function

  End Class

End Namespace
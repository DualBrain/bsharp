Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Binding
'Imports Ccl = Mono.Cecil
Imports Bsharp.CodeAnalysis.Symbols
'Imports Mono.Cecil.Cil
'Imports Mono.Cecil
'Imports Mono.Cecil.Rocks
'Imports Bsharp.CodeAnalysis.Syntax
Imports System.Text
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis.Emit

  ' This emitter is designed to handle Visual Basic.

  Friend NotInheritable Class VbEmitter

    Private ReadOnly _diagnostics As New DiagnosticBag
    'Private ReadOnly _knownTypes As New Dictionary(Of TypeSymbol, Ccl.TypeReference)
    'Private ReadOnly _objectEqualsReference As MethodReference
    'Private ReadOnly _consoleReadLineReference As MethodReference
    'Private ReadOnly _consoleWriteReference As MethodReference
    'Private ReadOnly _consoleWriteLineReference As MethodReference
    'Private ReadOnly _stringConcat2Reference As MethodReference
    'Private ReadOnly _stringConcat3Reference As MethodReference
    'Private ReadOnly _stringConcat4Reference As MethodReference
    'Private ReadOnly _stringConcatArrayReference As MethodReference
    'Private ReadOnly _convertToBooleanReference As MethodReference
    'Private ReadOnly _convertToInt32Reference As MethodReference
    'Private ReadOnly _convertToStringReference As MethodReference
    'Private ReadOnly _randomReference As TypeReference
    'Private ReadOnly _randomCtorReference As MethodReference
    'Private ReadOnly _randomNextReference As MethodReference
    'Private ReadOnly _assemblyDefinition As AssemblyDefinition
    'Private ReadOnly _methods As New Dictionary(Of FunctionSymbol, MethodDefinition)
    'Private ReadOnly _locals As New Dictionary(Of VariableSymbol, VariableDefinition)
    'Private ReadOnly _labels As New Dictionary(Of BoundLabel, Integer)
    'Private ReadOnly _fixups As New List(Of (InstructionIndex As Integer, Target As BoundLabel))

    'Private ReadOnly _typeDefinition As TypeDefinition
    'Private _randomFieldDefinition As FieldDefinition

    Private m_moduleName As String
    Private m_contents As String = ""

    Friend Sub New(moduleName As String, references() As String)

      m_moduleName = moduleName

      'Dim assemblies = New List(Of AssemblyDefinition)

      'For Each reference In references
      '  Try
      '    Dim assembly = Ccl.AssemblyDefinition.ReadAssembly(reference)
      '    assemblies.Add(assembly)
      '  Catch ex As BadImageFormatException
      '    _diagnostics.ReportInvalidReference(reference)
      '  End Try
      'Next

      'Dim builtInTypes = New List(Of (typeSymbol As TypeSymbol, metadataName As String)) From {
      '  (TypeSymbol.Any, "System.Object"),
      '  (TypeSymbol.Boolean, "System.Boolean"),
      '  (TypeSymbol.DateTime, "System.DateTime"),
      '  (TypeSymbol.Char, "System.Char"),
      '  (TypeSymbol.Decimal, "System.Decimal"),
      '  (TypeSymbol.Double, "System.Double"),
      '  (TypeSymbol.Single, "System.Single"),
      '  (TypeSymbol.ULong64, "System.UInt64"),
      '  (TypeSymbol.Long64, "System.Int64"),
      '  (TypeSymbol.ULong, "System.UInt32"),
      '  (TypeSymbol.Long, "System.Int32"),
      '  (TypeSymbol.UInteger, "System.UInt16"),
      '  (TypeSymbol.Integer, "System.Int16"),
      '  (TypeSymbol.SByte, "System.SByte"),
      '  (TypeSymbol.Byte, "System.Byte"),
      '  (TypeSymbol.String, "System.String"),
      '  (TypeSymbol.Nothing, "System.Void")
      '}

      'Dim assemblyName = New AssemblyNameDefinition(moduleName, New Version(1, 0))
      '_assemblyDefinition = AssemblyDefinition.CreateAssembly(assemblyName, moduleName, ModuleKind.Console)
      '_knownTypes = New Dictionary(Of TypeSymbol, TypeReference)

      'For Each entry In builtInTypes
      '  Dim typeReference = Emit_ResolveType(assemblies, entry.typeSymbol.Name, entry.metadataName)
      '  _knownTypes.Add(entry.typeSymbol, typeReference)
      'Next

      '_objectEqualsReference = Emit_ResolveMethod(assemblies, "System.Object", "Equals", {"System.Object", "System.Object"})
      ''_consoleReadLineReference = Emit_ResolveMethod(assemblies, "System.Console", "ReadLine", Array.Empty(Of String))
      '_consoleWriteReference = Emit_ResolveMethod(assemblies, "System.Console", "Write", {"System.Object"})
      ''_consoleWriteLineReference = Emit_ResolveMethod(assemblies, "System.Console", "WriteLine", {"System.Object"})
      '_consoleWriteLineReference = Emit_ResolveMethod(assemblies, "System.Console", "WriteLine", Array.Empty(Of String))
      '_stringConcat2Reference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String", "System.String"})
      '_stringConcat3Reference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String", "System.String", "System.String"})
      '_stringConcat4Reference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String", "System.String", "System.String", "System.String"})
      '_stringConcatArrayReference = Emit_ResolveMethod(assemblies, "System.String", "Concat", {"System.String[]"})
      '_convertToBooleanReference = Emit_ResolveMethod(assemblies, "System.Convert", "ToBoolean", {"System.Object"})
      '_convertToInt32Reference = Emit_ResolveMethod(assemblies, "System.Convert", "ToInt32", {"System.Object"})
      '_convertToStringReference = Emit_ResolveMethod(assemblies, "System.Convert", "ToString", {"System.Object"})

      '_randomReference = Emit_ResolveType(assemblies, Nothing, "System.Random")
      '_randomCtorReference = Emit_ResolveMethod(assemblies, "System.Random", ".ctor", Array.Empty(Of String))
      '_randomNextReference = Emit_ResolveMethod(assemblies, "System.Random", "Next", {"System.Int32"})

      'Dim objectType = _knownTypes(TypeSymbol.Any)
      'If objectType IsNot Nothing Then
      '  _typeDefinition = New TypeDefinition("", "Program", Ccl.TypeAttributes.Abstract Or Ccl.TypeAttributes.Sealed, objectType)
      '  _assemblyDefinition.MainModule.Types.Add(_typeDefinition)
      'Else
      '  _typeDefinition = Nothing
      'End If

    End Sub

    'Public Shared Function Emit(program As BoundProgram, moduleName As String, references() As String, outputPath As String) As ImmutableArray(Of Diagnostic)

    '  If program.ErrorDiagnostics.Any Then Return program.Diagnostics

    '  Dim emitter = New VbEmitter(moduleName, references)
    '  Return emitter.Emit(program, outputPath)

    'End Function

    Private m_tabs As Integer = 0

    Private Function Tab() As String
      Return Microsoft.VisualBasic.Space(m_tabs * 2)
    End Function

    Public Function Emit(program As BoundProgram, outputPath As String) As ImmutableArray(Of Diagnostic)

      If _diagnostics.Any Then Return _diagnostics.ToImmutableArray

      m_contents &= $"Option Explicit Off{vbCrLf}"
      m_contents &= $"Option Strict Off{vbCrLf}"
      m_contents &= $"Option Infer Off{vbCrLf}"
      m_contents &= $"{vbCrLf}"
      m_contents &= $"Imports System{vbCrLf}"
      m_contents &= $"{vbCrLf}"
      m_contents &= $"Namespace ConvertedFromBsharp{vbCrLf}"
      m_contents &= $"{vbCrLf}"
      m_tabs += 1
      m_contents &= $"{Tab()}Public Module {ProperCase(m_moduleName)}{vbCrLf}{vbCrLf}"
      m_tabs += 1
      For Each f In program.Functions
        EmitFunctionDeclaration(f.Key, f.Value)
        m_contents &= $"{vbCrLf}"
      Next
      m_tabs -= 1
      m_contents &= $"{Tab()}End Module{vbCrLf}"
      m_tabs -= 1
      m_contents &= $"{vbCrLf}"
      m_contents &= $"End Namespace{vbCrLf}"

      System.IO.File.WriteAllText(outputPath, m_contents)

      Return _diagnostics.ToImmutableArray

    End Function

    Private Sub EmitFunctionDeclaration(func As FunctionSymbol, body As BoundBlockStatement)
      Dim functionType = TypeSymbol.TypeSymbolToType(func.Type)
      Dim functionName = func.Name
      If functionType = TypeSymbol.Type.Nothing Then
        m_contents &= $"{Tab()}Public Sub {ProperCase(functionName)}("
      Else
        m_contents &= $"{Tab()}Public Function {ProperCase(functionName)}("
      End If
      For Each parameter In func.Parameters
        Dim parameterType = parameter.Type
        Dim parameterName = parameter.Name
        m_contents &= $"{parameterName} As {parameterType}, "
      Next
      If functionType = TypeSymbol.Type.Nothing Then
        m_contents &= $"){vbCrLf}"
      Else
        m_contents &= $") As {functionType}{vbCrLf}"
      End If
      m_tabs += 1
      EmitFunctionBody(func, body)
      m_tabs -= 1
      If functionType = TypeSymbol.Type.Nothing Then
        m_contents &= $"{Tab()}End Sub{vbCrLf}"
      Else
        m_contents &= $"{Tab()}End Function{vbCrLf}"
      End If
    End Sub

    Private Sub EmitFunctionBody(func As FunctionSymbol, body As BoundBlockStatement)
      For Each statement In body.Statements
        EmitStatement(statement)
      Next
    End Sub

    Private Sub EmitStatement(node As BoundStatement)
      Select Case node.Kind
        'Case BoundNodeKind.ConditionalGotoStatement : EmitConditionalGotoStatement(ilProcessor, CType(node, BoundConditionalGotoStatement))
        Case BoundNodeKind.GotoStatement : EmitGotoStatement(CType(node, BoundGotoStatement))
        'Case BoundNodeKind.ExpressionStatement : EmitExpressionStatement(ilProcessor, CType(node, BoundExpressionStatement))
        'Case BoundNodeKind.HandleCommaStatement : EmitHandleCommaStatement(ilProcessor, CType(node, BoundHandleCommaStatement))
        Case BoundNodeKind.HandlePrintLineStatement : EmitHandlePrintLineStatement(CType(node, BoundHandlePrintLineStatement))
        Case BoundNodeKind.HandlePrintStatement : EmitHandlePrintStatement(CType(node, BoundHandlePrintStatement))
        'Case BoundNodeKind.HandleSpcStatement : EmitHandleSpcStatement(ilProcessor, CType(node, BoundHandleSpcStatement))
        'Case BoundNodeKind.HandleTabStatement : EmitHandleTabStatement(ilProcessor, CType(node, BoundHandleTabStatement))
        Case BoundNodeKind.LetStatement : EmitLetStatement(CType(node, BoundLetStatement))
        Case BoundNodeKind.LabelStatement : EmitLabelStatement(CType(node, BoundLabelStatement))
          'Case BoundNodeKind.NopStatement : EmitNopStatement(ilProcessor, CType(node, BoundNopStatement))
          'Case BoundNodeKind.RemStatement : EmitRemStatement(ilProcessor, CType(node, BoundRemStatement))
          'Case BoundNodeKind.PrintStatement : EmitPrintStatement(CType(node, BoundPrintStatement))
        Case BoundNodeKind.ReturnStatement : EmitReturnStatement(CType(node, BoundReturnStatement))
          'Case BoundNodeKind.VariableDeclaration : EmitVariableDeclaration(ilProcessor, CType(node, BoundVariableDeclaration))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    Private Sub EmitAssignmentExpression(node As BoundAssignmentExpression)
      'Dim variableDefinition = _locals(node.Variable)
      'EmitExpression(ilProcessor, node.Expression)
      'ilProcessor.Emit(OpCodes.Dup)
      'ilProcessor.Emit(OpCodes.Stloc, variableDefinition)
      Stop
    End Sub

    Private Sub EmitBinaryExpression(node As BoundBinaryExpression)

      ' +(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.Addition Then
        If node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String Then
          'EmitStringConcatExpression(ilProcessor, node)
          Stop
          Return
        End If
      End If

      EmitExpression(node.Left)

      ' ==(any, any)
      ' ==(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.Equal Then
        If (node.Left.Type Is TypeSymbol.Any AndAlso node.Right.Type Is TypeSymbol.Any) OrElse
           (node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String) Then
          'ilProcessor.Emit(OpCodes.Call, _objectEqualsReference)
          Stop
          Return
        End If
      End If

      ' !=(any, any)
      ' !=(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.NotEqual Then
        If (node.Left.Type Is TypeSymbol.Any AndAlso node.Right.Type Is TypeSymbol.Any) OrElse
           (node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String) Then
          'ilProcessor.Emit(OpCodes.Call, _objectEqualsReference)
          'ilProcessor.Emit(OpCodes.Ldc_I4_0)
          'ilProcessor.Emit(OpCodes.Ceq)
          Stop
          Return
        End If
      End If

      Select Case node.Op.Kind
        Case BoundBinaryOperatorKind.Addition
          m_contents &= " + "
        Case BoundBinaryOperatorKind.Subtraction
          m_contents &= " - "
        Case BoundBinaryOperatorKind.Multiplication
          m_contents &= " * "
        Case BoundBinaryOperatorKind.Division
          m_contents &= " / "
        Case BoundBinaryOperatorKind.IntegerDivision
          m_contents &= " \ "
        Case BoundBinaryOperatorKind.LogicalAnd
          m_contents &= " AndAlso "
        Case BoundBinaryOperatorKind.LogicalOr
          m_contents &= " OrElse "
        Case BoundBinaryOperatorKind.BitwiseAnd
          m_contents &= " And "
        Case BoundBinaryOperatorKind.BitwiseOr
          m_contents &= " Or "
        Case BoundBinaryOperatorKind.BitwiseXor
          m_contents &= " Xor "
        Case BoundBinaryOperatorKind.Equal
          m_contents &= " = "
        Case BoundBinaryOperatorKind.NotEqual
          m_contents &= " <> "
        Case BoundBinaryOperatorKind.LessThan
          m_contents &= " < "
        Case BoundBinaryOperatorKind.LessThanEqual
          m_contents &= " <= "
        Case BoundBinaryOperatorKind.GreaterThan
          m_contents &= " > "
        Case BoundBinaryOperatorKind.GreaterThanEqual
          m_contents &= " >= "
        Case Else
          Throw New Exception($"Unexpected binary operator {SyntaxFacts.GetText(node.Op.SyntaxKind)}({node.Op.Type})")
      End Select

      EmitExpression(node.Right)

    End Sub

    Private Sub EmitCallExpression(node As BoundCallExpression)

      Stop

      'If node.Function Is BuiltinFunctions.Rnd2 Then

      '  If _randomFieldDefinition Is Nothing Then
      '    EmitRandomField()
      '  End If

      '  ilProcessor.Emit(OpCodes.Ldsfld, _randomFieldDefinition)
      '  For Each argument In node.Arguments
      '    EmitExpression(ilProcessor, argument)
      '  Next
      '  ilProcessor.Emit(OpCodes.Callvirt, _randomNextReference)

      '  Return

      'End If

      'For Each argument In node.Arguments
      '  EmitExpression(ilProcessor, argument)
      'Next

      ''If node.Function Is Input Then
      ''  ilProcessor.Emit(OpCodes.Call, _consoleReadLineReference)
      ''  'ElseIf node.Function Is Print Then
      ''  '  ilProcessor.Emit(OpCodes.Call, _consoleWriteLineReference)
      ''Else
      'Dim methodDefinition = _methods(node.Function)
      'ilProcessor.Emit(OpCodes.Call, methodDefinition)
      ''End If

    End Sub

    'Private Sub EmitConditionalGotoStatement(ilProcessor As ILProcessor, node As BoundConditionalGotoStatement)
    '  EmitExpression(ilProcessor, node.Condition)
    '  Dim opCode = If(node.JumpIfTrue, OpCodes.Brtrue, OpCodes.Brfalse)
    '  _fixups.Add((ilProcessor.Body.Instructions.Count, node.Label))
    '  ilProcessor.Emit(opCode, Instruction.Create(OpCodes.Nop))
    'End Sub

    Private Sub EmitConstantExpression(node As BoundExpression)
      Debug.Assert(node.ConstantValue IsNot Nothing)
      If node.Type Is TypeSymbol.Boolean Then
        Dim value = CBool(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.Long64 Then
        Dim value = CLng(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.Integer Then
        Dim value = CShort(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.Long Then
        Dim value = CInt(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.String Then
        Dim value = CStr(node.ConstantValue.Value)
        m_contents &= $"""{value}"""
      Else
        Throw New Exception($"Unexpected constant expression type: {node.Type}")
      End If
    End Sub

    Private Sub EmitConversionExpression(node As BoundConversionExpression)
      'EmitExpression(node.Expression)
      'Dim needsBoxing = node.Expression.Type Is TypeSymbol.Boolean OrElse
      '                  node.Expression.Type Is TypeSymbol.Integer
      'If needsBoxing Then ilProcessor.Emit(OpCodes.Box, _knownTypes(node.Expression.Type))
      If node.Type Is TypeSymbol.Any Then
        ' Done.
        EmitExpression(node.Expression)
      ElseIf node.Type Is TypeSymbol.Boolean Then
        'm_contents &= $"CBool(" : EmitExpression(node.Expression) : m_contents &= $")"
        EmitExpression(node.Expression)
      ElseIf node.Type Is TypeSymbol.Integer Then
        'm_contents &= $"CInt(" : EmitExpression(node.Expression) : m_contents &= $")"
        EmitExpression(node.Expression)
      ElseIf node.Type Is TypeSymbol.Double Then
        'm_contents &= $"CDbl(" : EmitExpression(node.Expression) : m_contents &= $")"
        EmitExpression(node.Expression)
      ElseIf node.Type Is TypeSymbol.String Then
        'm_contents &= $"CStr(" : EmitExpression(node.Expression) : m_contents &= $")"
        EmitExpression(node.Expression)
      Else
        Throw New Exception($"Unexpected conversion from {node.Expression.Type} to {node.Type}")
      End If
    End Sub

    Private Sub EmitExpression(node As BoundExpression)

      If node.ConstantValue IsNot Nothing Then
        EmitConstantExpression(node)
        Return
      End If

      Select Case node.Kind
        Case BoundNodeKind.VariableExpression : EmitVariableExpression(CType(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : EmitAssignmentExpression(CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : EmitUnaryExpression(CType(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : EmitBinaryExpression(CType(node, BoundBinaryExpression))
        Case BoundNodeKind.CallExpression : EmitCallExpression(CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : EmitConversionExpression(CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    'Private Sub EmitExpressionStatement(ilProcessor As ILProcessor, node As BoundExpressionStatement)
    '  EmitExpression(ilProcessor, node.Expression)
    '  If node.Expression.Type IsNot TypeSymbol.Nothing Then
    '    ilProcessor.Emit(OpCodes.Pop)
    '  End If
    'End Sub

    Private Sub EmitGotoStatement(node As BoundGotoStatement)
      Dim label = node.Label.Name
      If label.StartsWith("$") Then
        label = label.Substring(1)
      End If
      m_contents &= $"{Tab()}Goto {label}{vbCrLf}"
    End Sub

    'Private Shared Sub EmitHandleCommaStatement(ilProcessor As ILProcessor, node As BoundHandleCommaStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    Private Sub EmitHandlePrintLineStatement(node As BoundHandlePrintLineStatement)
      m_contents &= $"{Tab()}System.Console.WriteLine(){vbCrLf}"
    End Sub

    Private Sub EmitHandlePrintStatement(node As BoundHandlePrintStatement)
      m_contents &= $"{Tab()}System.Console.Write("
      EmitExpression(node.Expression)
      m_contents &= $"){vbCrLf}"
    End Sub

    'Private Shared Sub EmitHandleSpcStatement(ilProcessor As ILProcessor, node As BoundHandleSpcStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    'Private Shared Sub EmitHandleTabStatement(ilProcessor As ILProcessor, node As BoundHandleTabStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    Private Sub EmitLetStatement(node As BoundLetStatement)
      'm_contents &= $"{Tab()}Dim {node.Variable.Name} = "
      m_contents &= $"{Tab()}{node.Variable.Name} = "
      EmitExpression(node.Expression)
      m_contents &= $"{vbCrLf}"
    End Sub

    Private Sub EmitLabelStatement(node As BoundLabelStatement)
      Dim label = node.Label.Name
      If label.StartsWith("$") Then
        label = label.Substring(1)
      End If
      m_contents &= $"{label}:{vbCrLf}"
    End Sub

    'Private Shared Sub EmitNopStatement(ilProcessor As ILProcessor, node As BoundNopStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    'Private Shared Sub EmitRemStatement(ilProcessor As ILProcessor, node As BoundRemStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    Private Sub EmitPrintStatement(node As BoundPrintStatement)
      m_contents &= $"{Tab()}' PRINT ????{vbCrLf}"
    End Sub

    'Private Sub EmitRandomField()
    '  _randomFieldDefinition = New FieldDefinition("$rnd", Ccl.FieldAttributes.Static Or
    '                                                       Ccl.FieldAttributes.Private, _randomReference)
    '  _typeDefinition.Fields.Add(_randomFieldDefinition)
    '  Dim staticConstructor = New Ccl.MethodDefinition(".cctor", Ccl.MethodAttributes.Static Or
    '                                                             Ccl.MethodAttributes.Private Or
    '                                                             Ccl.MethodAttributes.SpecialName Or
    '                                                             Ccl.MethodAttributes.RTSpecialName, _knownTypes(TypeSymbol.Nothing))
    '  _typeDefinition.Methods.Insert(0, staticConstructor)
    '  Dim ilProcessor = staticConstructor.Body.GetILProcessor
    '  ilProcessor.Emit(OpCodes.Newobj, _randomCtorReference)
    '  ilProcessor.Emit(OpCodes.Stsfld, _randomFieldDefinition)
    '  ilProcessor.Emit(OpCodes.Ret)
    'End Sub

    Private Sub EmitReturnStatement(node As BoundReturnStatement)
      m_contents &= $"{Tab()}Return"
      If node.Expression IsNot Nothing Then m_contents &= " " : EmitExpression(node.Expression)
      m_contents &= $"{vbCrLf}"
    End Sub

    'Private Sub EmitStringConcatExpression(_ilProcessor As ILProcessor, node As BoundBinaryExpression)

    '  ' Flatten the expression tree to a sequence of nodes to concatenate, then fold consecutive constants in that sequence.
    '  ' This approach enables constant folding of non-sibling nodes, which cannot be done in the ConstantFolding class as it would require changing the tree.
    '  ' Example: folding b And c in ((a + b) + c) if they are constant.

    '  Dim nodes = EmitStringConcatExpression_FoldConstants(EmitStringConcatExpression_Flatten(node)).ToList()

    '  Select Case nodes.Count
    '    Case 0
    '      _ilProcessor.Emit(OpCodes.Ldstr, String.Empty)

    '    Case 1
    '      EmitExpression(_ilProcessor, nodes(0))

    '    Case 2
    '      EmitExpression(_ilProcessor, nodes(0))
    '      EmitExpression(_ilProcessor, nodes(1))
    '      _ilProcessor.Emit(OpCodes.[Call], _stringConcat2Reference)

    '    Case 3
    '      EmitExpression(_ilProcessor, nodes(0))
    '      EmitExpression(_ilProcessor, nodes(1))
    '      EmitExpression(_ilProcessor, nodes(2))
    '      _ilProcessor.Emit(OpCodes.[Call], _stringConcat3Reference)

    '    Case 4
    '      EmitExpression(_ilProcessor, nodes(0))
    '      EmitExpression(_ilProcessor, nodes(1))
    '      EmitExpression(_ilProcessor, nodes(2))
    '      EmitExpression(_ilProcessor, nodes(3))
    '      _ilProcessor.Emit(OpCodes.[Call], _stringConcat4Reference)

    '    Case Else
    '      _ilProcessor.Emit(OpCodes.Ldc_I4, nodes.Count)
    '      _ilProcessor.Emit(OpCodes.Newarr, _knownTypes(TypeSymbol.[String]))
    '      For i = 0 To nodes.Count - 1
    '        _ilProcessor.Emit(OpCodes.Dup)
    '        _ilProcessor.Emit(OpCodes.Ldc_I4, i)
    '        EmitExpression(_ilProcessor, nodes(i))
    '        _ilProcessor.Emit(OpCodes.Stelem_Ref)
    '      Next
    '      _ilProcessor.Emit(OpCodes.[Call], _stringConcatArrayReference)

    '  End Select

    'End Sub

    'Private Iterator Function EmitStringConcatExpression_Flatten(node As BoundExpression) As IEnumerable(Of BoundExpression)

    '  Dim binaryExpression = TryCast(node, BoundBinaryExpression)
    '  If binaryExpression IsNot Nothing AndAlso
    '     binaryExpression.Op.Kind = BoundBinaryOperatorKind.Addition AndAlso
    '     binaryExpression.Left.Type Is TypeSymbol.[String] AndAlso
    '     binaryExpression.Right.Type Is TypeSymbol.[String] Then

    '    For Each result In EmitStringConcatExpression_Flatten(binaryExpression.Left)
    '      Yield result
    '    Next

    '    For Each result In EmitStringConcatExpression_Flatten(binaryExpression.Right)
    '      Yield result
    '    Next

    '  Else

    '    If node.Type IsNot TypeSymbol.[String] Then
    '      Throw New Exception($"Unexpected node type in string concatenation: {node.Type}")
    '    End If

    '    Yield node

    '  End If

    'End Function

    'Private Shared Iterator Function EmitStringConcatExpression_FoldConstants(nodes As IEnumerable(Of BoundExpression)) As IEnumerable(Of BoundExpression)

    '  Dim sb As StringBuilder = Nothing

    '  For Each node In nodes
    '    If node.ConstantValue IsNot Nothing Then
    '      Dim stringValue = CStr(node.ConstantValue.Value)

    '      If String.IsNullOrEmpty(stringValue) Then
    '        Continue For
    '      End If

    '      sb = If(sb, New StringBuilder)
    '      sb.Append(stringValue)

    '    Else

    '      If sb?.Length > 0 Then
    '        Yield New BoundLiteralExpression(sb.ToString())
    '        sb.Clear()
    '      End If

    '      Yield node

    '    End If
    '  Next

    '  If sb?.Length > 0 Then
    '    Yield New BoundLiteralExpression(sb.ToString())
    '  End If

    'End Function

    Private Sub EmitUnaryExpression(node As BoundUnaryExpression)
      'EmitExpression(ilProcessor, node.Operand)
      'If node.Op.Kind = BoundUnaryOperatorKind.Identity Then
      '  ' Done.
      'ElseIf node.Op.Kind = BoundUnaryOperatorKind.LogicalNegation Then
      '  ilProcessor.Emit(OpCodes.Ldc_I4_0)
      '  ilProcessor.Emit(OpCodes.Ceq)
      'ElseIf node.Op.Kind = BoundUnaryOperatorKind.Negation Then
      '  ilProcessor.Emit(OpCodes.Neg)
      'ElseIf node.Op.Kind = BoundUnaryOperatorKind.BitwiseComplement Then
      '  ilProcessor.Emit(OpCodes.Not)
      'Else
      '  Throw New Exception($"Unexpected unary operator {SyntaxFacts.GetText(node.Op.SyntaxKind)}({node.Operand.Type})")
      'End If
      Stop
    End Sub

    'Private Sub EmitVariableDeclaration(ilProcessor As ILProcessor, node As BoundVariableDeclaration)
    '  Dim typeReference = _knownTypes(node.Variable.Type)
    '  Dim variableDefinition = New VariableDefinition(typeReference)
    '  _locals.Add(node.Variable, variableDefinition)
    '  ilProcessor.Body.Variables.Add(variableDefinition)
    '  EmitExpression(ilProcessor, node.Initializer)
    '  ilProcessor.Emit(OpCodes.Stloc, variableDefinition)
    'End Sub

    Private Sub EmitVariableExpression(node As BoundVariableExpression)
      If TypeOf node.Variable Is ParameterSymbol Then
        Dim parameter = CType(node.Variable, ParameterSymbol)
        m_contents &= $"{parameter}"
      Else
        m_contents &= $"{node.Variable.Name}"
      End If
    End Sub

#Region "Converted from 'inline' functions."

    Private Function ProperCase(value As String) As String
      If "abcdefghijklmnopqrstuvwxyz".Contains(value(0)) Then
        Return value(0).ToString.ToUpper & value.Substring(1)
      Else
        Return value
      End If
    End Function

    'Private Function Emit_ResolveType(assemblies As List(Of AssemblyDefinition),
    '                                  internalName As String,
    '                                  metadataName As String) As TypeReference
    '  Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
    '                              SelectMany(Function(m) m.Types).
    '                              Where(Function(t) t.FullName = metadataName).ToArray
    '  If foundTypes.Length = 1 Then
    '    Dim typeReference = _assemblyDefinition.MainModule.ImportReference(foundTypes(0))
    '    Return typeReference
    '  ElseIf foundTypes.Length = 0 Then
    '    _diagnostics.ReportRequiredTypeNotFound(internalName, metadataName)
    '  Else
    '    _diagnostics.ReportRequiredTypeAmbiguous(internalName, metadataName, foundTypes)
    '  End If
    '  Return Nothing
    'End Function

    'Private Function Emit_ResolveMethod(assemblies As List(Of AssemblyDefinition),
    '                                    typeName As String,
    '                                    methodName As String,
    '                                    parameterTypeNames As String()) As MethodReference
    '  Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
    '                              SelectMany(Function(m) m.Types).
    '                              Where(Function(t) t.FullName = typeName).ToArray
    '  If foundTypes.Length = 1 Then
    '    Dim foundType = foundTypes(0)
    '    Dim methods = foundType.Methods.Where(Function(m) m.Name = methodName)

    '    For Each method In methods
    '      If method.Parameters.Count <> parameterTypeNames.Length Then
    '        Continue For
    '      End If
    '      Dim allParametersMatch = True
    '      For i = 0 To parameterTypeNames.Length - 1
    '        If method.Parameters(i).ParameterType.FullName <> parameterTypeNames(i) Then
    '          allParametersMatch = False
    '          Exit For
    '        End If
    '      Next
    '      If Not allParametersMatch Then
    '        Continue For
    '      End If
    '      Return _assemblyDefinition.MainModule.ImportReference(method)
    '    Next
    '    _diagnostics.ReportRequiredMethodNotFound(typeName, methodName, parameterTypeNames)
    '    Return Nothing
    '  ElseIf foundTypes.Length = 0 Then
    '    _diagnostics.ReportRequiredTypeNotFound(Nothing, typeName)
    '  Else
    '    _diagnostics.ReportRequiredTypeAmbiguous(Nothing, typeName, foundTypes)
    '  End If
    '  Return Nothing
    'End Function

#End Region

  End Class

End Namespace
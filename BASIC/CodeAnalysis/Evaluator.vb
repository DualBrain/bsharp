Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis

  ' TODO: Get rid of evaluator in favor of Emitter (see #113)
  Friend NotInheritable Class Evaluator

    Private ReadOnly m_program As BoundProgram
    Private ReadOnly m_globals As Dictionary(Of VariableSymbol, Object)
    Private ReadOnly m_functions As New Dictionary(Of FunctionSymbol, BoundBlockStatement)
    Private ReadOnly m_locals As New Stack(Of Dictionary(Of VariableSymbol, Object))
    Private m_random As Random

    Private m_lastValue As Object

    Sub New(program As BoundProgram, variables As Dictionary(Of VariableSymbol, Object))

      m_program = program
      m_globals = variables
      m_locals.Push(New Dictionary(Of VariableSymbol, Object))

      Dim currrent = program
      While currrent IsNot Nothing
        For Each kv In currrent.Functions
          Dim func = kv.Key
          Dim body = kv.Value
          m_functions.Add(func, body)
        Next
        currrent = currrent.Previous
      End While

    End Sub

    Public Function Evaluate() As Object
      Dim func = If(m_program.MainFunction, m_program.ScriptFunction)
      If func Is Nothing Then
        Return Nothing
      Else
        Dim body = m_functions(func)
        Return EvaluateStatement(body)
      End If
    End Function

    Private Function EvaluateStatement(body As BoundBlockStatement) As Object

      Dim labelToIndex = New Dictionary(Of BoundLabel, Integer)

      For i = 0 To body.Statements.Length - 1
        If TypeOf body.Statements(i) Is BoundLabelStatement Then
          labelToIndex.Add(CType(body.Statements(i), BoundLabelStatement).Label, i + 1)
        End If
      Next

      Dim index = 0
      While index < body.Statements.Length
        Dim s = body.Statements(index)
        Select Case s.Kind
          Case BoundNodeKind.PrintStatement
            EvaluatePrintStatement(CType(s, BoundPrintStatement)) : index += 1
          Case BoundNodeKind.NopStatement : index += 1
          Case BoundNodeKind.VariableDeclaration
            EvaluateVariableDeclaration(CType(s, BoundVariableDeclaration)) : index += 1
          Case BoundNodeKind.ExpressionStatement
            EvaluateExpressionStatement(CType(s, BoundExpressionStatement)) : index += 1
          Case BoundNodeKind.GotoStatement
            Dim gs = CType(s, BoundGotoStatement)
            index = labelToIndex(gs.Label)
          Case BoundNodeKind.ConditionalGotoStatement
            Dim cgs = CType(s, BoundConditionalGotoStatement)
            Dim condition = CBool(EvaluateExpression(cgs.Condition))
            If condition = cgs.JumpIfTrue Then
              index = labelToIndex(cgs.Label)
            Else
              index += 1
            End If
          Case BoundNodeKind.LabelStatement : index += 1
          Case BoundNodeKind.ReturnStatement
            Dim rs = CType(s, BoundReturnStatement)
            m_lastValue = If(rs.Expression Is Nothing, Nothing, EvaluateExpression(rs.Expression))
            Return m_lastValue
          Case Else
            Throw New Exception($"Unexpected kind {s.Kind}")
        End Select
      End While

      Return m_lastValue

    End Function

    Private Sub EvaluatePrintStatement(s As BoundPrintStatement)
      Dim cr = True
      For Each entry In s.Nodes
        If TypeOf entry Is BoundSymbol Then
          Select Case CType(entry, BoundSymbol).Value
            Case ";"c
              cr = False
            Case ","
              'TODO: Need to determine where the current character
              '      position is and then determine how many spaces
              '      need to be printed so that the cursor is at the
              '      next 14 character offset.
              Console.Write("              ") ' for now, 14 characters - see above.
              cr = False
            Case Else
              Throw New NotImplementedException
              cr = True
          End Select
        Else
          Dim value = EvaluateExpression(CType(entry, BoundExpression))
          Dim str = CStr(value)
          Console.Write(str)
          cr = True
        End If
      Next
      If cr Then
        Console.WriteLine()
      End If
    End Sub

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value = EvaluateExpression(node.Initializer)
      Debug.Assert(value IsNot Nothing)
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    Private Sub EvaluateExpressionStatement(node As BoundExpressionStatement)
      m_lastValue = EvaluateExpression(node.Expression)
    End Sub

    Private Function EvaluateExpression(node As BoundExpression) As Object

      If node.ConstantValue IsNot Nothing Then
        Return EvaluateConstantExpression(node)
      End If

      Select Case node.Kind
        'Case BoundNodeKind.LiteralExpression : Return EvaluateLiteralExpression(CType(node, BoundLiteralExpression))
        Case BoundNodeKind.VariableExpression : Return EvaluateVariableExpression(CType(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : Return EvaluateAssignmentExpression(CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : Return EvaluateUnaryExpression(CType(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : Return EvaluateBinaryExpression(CType(node, BoundBinaryExpression))
        Case BoundNodeKind.CallExpression : Return EvaluateCallExpression(CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : Return EvaluateConversionExpression(CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select

    End Function

    Private Function EvaluateConstantExpression(node As BoundExpression) As Object
      Debug.Assert(node.ConstantValue IsNot Nothing)
      Return node.ConstantValue.Value
    End Function

    Private Function EvaluateVariableExpression(node As BoundVariableExpression) As Object
      If node.Variable.Kind = SymbolKind.GlobalVariable Then
        Return m_globals(node.Variable)
      Else
        Dim locals = m_locals.Peek
        Return locals(node.Variable)
      End If
    End Function

    Private Function EvaluateAssignmentExpression(node As BoundAssignmentExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      Debug.Assert(value IsNot Nothing)
      Assign(node.Variable, value)
      Return value
    End Function

    Private Function EvaluateUnaryExpression(node As BoundUnaryExpression) As Object
      Dim operand = EvaluateExpression(node.Operand)
      Select Case node.Op.Kind
        Case BoundUnaryOperatorKind.Identity : Return CInt(operand)
        Case BoundUnaryOperatorKind.Negation : Return -CInt(operand)
        Case BoundUnaryOperatorKind.LogicalNegation : Return Not CBool(operand)
        Case BoundUnaryOperatorKind.BitwiseComplement : Return Not CInt(operand)
        Case Else
          Throw New Exception($"Unexpected unary operator {node.Op}")
      End Select
    End Function

    Private Function EvaluateBinaryExpression(node As BoundBinaryExpression) As Object
      Dim left = EvaluateExpression(node.Left)
      Dim right = EvaluateExpression(node.Right)
      Debug.Assert(left IsNot Nothing AndAlso right IsNot Nothing)
      Select Case node.Op.Kind

' 14 ()
' 13 ^
' 12 - (negation "unary")
' 11 */
' 10 \
' 09 MOD
' 08 +-
' 07 = > >= < <= <>
' 06 NOT
' 05 AND, AndAlso
' 04 OR, OrElse
' 03 XOR
' 02 EQV
' 01 IMP

        Case BoundBinaryOperatorKind.Raise : Return CInt(left) ^ CInt(right)

        Case BoundBinaryOperatorKind.Multiplication : Return CInt(left) * CInt(right)
        Case BoundBinaryOperatorKind.Division : Return CInt(CInt(left) / CInt(right))
        Case BoundBinaryOperatorKind.IntegerDivision : Return CInt(left) \ CInt(right)
        Case BoundBinaryOperatorKind.ModOperation : Return CInt(left) Mod CInt(right)
        Case BoundBinaryOperatorKind.Addition
          If node.Type Is TypeSymbol.String Then
            Return CStr(left) + CStr(right)
          ElseIf node.Type Is TypeSymbol.Integer Then
            Return CInt(left) + CInt(right)
          Else
            Throw New Exception($"Unexpected binary operator {node.Op.Kind} type {node.Type}.")
          End If

        Case BoundBinaryOperatorKind.Subtraction : Return CInt(left) - CInt(right)

        Case BoundBinaryOperatorKind.Equal : Return Equals(left, right)
        Case BoundBinaryOperatorKind.GreaterThan : Return CInt(left) > CInt(right)
        Case BoundBinaryOperatorKind.GreaterThanEqual : Return CInt(left) >= CInt(right)
        Case BoundBinaryOperatorKind.LessThan : Return CInt(left) < CInt(right)
        Case BoundBinaryOperatorKind.LessThanEqual : Return CInt(left) <= CInt(right)
        Case BoundBinaryOperatorKind.NotEqual : Return Not Equals(left, right)

        Case BoundBinaryOperatorKind.LogicalAnd
          If node.Type Is TypeSymbol.Boolean Then
            Return CBool(left) And CBool(right)
          ElseIf node.Type Is TypeSymbol.Integer Then
            Return CInt(left) And CInt(right)
          Else
            Throw New Exception($"Unexpected binary operator {node.Op.Kind} type {node.Type}.")
          End If

        Case BoundBinaryOperatorKind.BitwiseAnd
          If node.Type Is TypeSymbol.Integer Then
            Return CInt(left) And CInt(right)
          Else
            Return CBool(left) And CBool(right)
          End If
        Case BoundBinaryOperatorKind.LogicalAndAlso : Return CBool(left) AndAlso CBool(right)
        Case BoundBinaryOperatorKind.LogicalOr : Return CBool(left) Or CBool(right)
        Case BoundBinaryOperatorKind.BitwiseOr
          If node.Type Is TypeSymbol.Integer Then
            Return CInt(left) Or CInt(right)
          Else
            Return CBool(left) Or CBool(right)
          End If
        Case BoundBinaryOperatorKind.LogicalOrElse : Return CBool(left) OrElse CBool(right)

        Case BoundBinaryOperatorKind.BitwiseXor : Return CInt(left) Xor CInt(right)
        Case BoundBinaryOperatorKind.LogicalXor : Return CBool(left) Xor CBool(right)
        Case BoundBinaryOperatorKind.BitwiseEqv : Return CBool(left) = CBool(right)
        Case BoundBinaryOperatorKind.BitwiseImp : Return CBool(left) AndAlso Not CBool(right)

        Case Else
          Throw New Exception($"Unexpected binary operator {node.Op.Kind}")
      End Select
    End Function

    Private Function EvaluateCallExpression(node As BoundCallExpression) As Object
      If node.[Function] Is BuiltinFunctions.Input Then
        Return Console.ReadLine()
        'ElseIf node.[Function] Is BuiltinFunctions.Print Then
        '  Dim message = CStr(EvaluateExpression(node.Arguments(0)))
        '  Console.WriteLine(message)
        '  Return Nothing
      ElseIf node.[Function] Is BuiltinFunctions.Rnd Then
        Dim max = CInt(EvaluateExpression(node.Arguments(0)))
        If m_random Is Nothing Then m_random = New Random
        Return m_random.[Next](max)
      Else
        Dim locals = New Dictionary(Of VariableSymbol, Object)
        For i = 0 To node.Arguments.Length - 1
          Dim parameter = node.Function.Parameters(i)
          Dim value = EvaluateExpression(node.Arguments(i))
          Debug.Assert(value IsNot Nothing)
          locals.Add(parameter, value)
        Next
        m_locals.Push(locals)
        Dim statement = m_functions(node.Function)
        Dim result = EvaluateStatement(statement)
        m_locals.Pop()
        Return result
      End If
    End Function

    Private Function EvaluateConversionExpression(node As BoundConversionExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      If node.Type Is TypeSymbol.Any Then
        Return value
      ElseIf node.Type Is TypeSymbol.Boolean Then
        Return Convert.ToBoolean(value)
      ElseIf node.Type Is TypeSymbol.Integer Then
        Return Convert.ToInt32(value)
      ElseIf node.Type Is TypeSymbol.String Then
        Return Convert.ToString(value)
      Else
        Throw New Exception($"Unexpected type {node.Type}")
      End If
    End Function

    Private Sub Assign(variable As VariableSymbol, value As Object)
      If variable.Kind = SymbolKind.GlobalVariable Then
        m_globals(variable) = value
      Else
        Dim locals = m_locals.Peek
        locals(variable) = value
      End If
    End Sub

  End Class

  '' This Evaluator walks the "raw" SyntaxTree.
  'Public NotInheritable Class Evaluator_SyntaxTree

  '  Private ReadOnly m_root As ExpressionSyntax

  '  Sub New(root As ExpressionSyntax)
  '    m_root = root
  '  End Sub

  '  Public Function Evaluate() As Integer
  '    Return EvaluateExpression(m_root)
  '  End Function

  '  Private Function EvaluateExpression(node As ExpressionSyntax) As Integer

  '    If TypeOf node Is LiteralExpressionSyntax Then
  '      Return CInt(CType(node, LiteralExpressionSyntax).LiteralToken.Value)
  '    End If

  '    If TypeOf node Is UnaryExpressionSyntax Then
  '      Dim u = CType(node, UnaryExpressionSyntax)
  '      Dim operand = EvaluateExpression(u.Operand)
  '      Select Case u.OperatorToken.Kind
  '        Case SyntaxKind.PlusToken : Return operand
  '        Case SyntaxKind.MinusToken : Return -operand
  '        Case Else
  '          Throw New Exception($"Unexpected unary operator {u.OperatorToken.Kind}")
  '      End Select
  '    End If

  '    If TypeOf node Is BinaryExpressionSyntax Then
  '      Dim b = CType(node, BinaryExpressionSyntax)
  '      Dim left = EvaluateExpression(b.Left)
  '      Dim right = EvaluateExpression(b.Right)
  '      Select Case b.OperatorToken.Kind
  '        Case SyntaxKind.PlusToken : Return left + right
  '        Case SyntaxKind.MinusToken : Return left - right
  '        Case SyntaxKind.StarToken : Return left * right
  '        Case SyntaxKind.SlashToken : Return CInt(left / right)
  '        Case SyntaxKind.BackslashToken : Return left \ right
  '        Case Else
  '          Throw New Exception($"Unexpected binary operator {b.OperatorToken.Kind}")
  '      End Select
  '    End If

  '    If TypeOf node Is ParenExpressionSyntax Then
  '      Dim p = CType(node, ParenExpressionSyntax)
  '      Return EvaluateExpression(p.Expression)
  '    End If

  '    Throw New Exception($"Unexpected node {node.Kind}")

  '  End Function

  'End Class

End Namespace
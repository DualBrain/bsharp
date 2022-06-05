Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis

  ' This Evaluator utilizes the "Bound Tree" (very similar to the AST).
  Friend NotInheritable Class Evaluator

    'Private ReadOnly m_root As BoundBlockStatement
    'Private ReadOnly m_variables As Dictionary(Of VariableSymbol, Object)
    Private ReadOnly m_program As BoundProgram
    Private ReadOnly m_globals As Dictionary(Of VariableSymbol, Object)
    Private ReadOnly m_locals As New Stack(Of Dictionary(Of VariableSymbol, Object))
    Private m_random As Random

    Private m_lastValue As Object

    Sub New(program As BoundProgram, variables As Dictionary(Of VariableSymbol, Object))
      'm_root = root
      'm_variables = variables
      m_program = program
      m_globals = variables
      m_locals.Push(New Dictionary(Of VariableSymbol, Object))
    End Sub

    Public Function Evaluate() As Object
      Return EvaluateStatement(m_program.Statement)
    End Function

    Private Function EvaluateStatement(body As BoundBlockStatement) As Object

      Dim labelToIndex = New Dictionary(Of BoundLabel, Integer)

      For i = 0 To body.Statements.Length - 1
        If TypeOf body.Statements(i) Is BoundLabelStatement Then
          Dim l = CType(body.Statements(i), BoundLabelStatement)
          labelToIndex.Add(l.Label, i + 1)
        End If
      Next

      Dim index = 0
      While index < body.Statements.Length
        Dim s = body.Statements(index)
        Select Case s.Kind
          Case BoundNodeKind.VariableDeclaration
            EvaluateVariableDeclaration(CType(s, BoundVariableDeclaration)) : index += 1
          'Case BoundNodeKind.SelectCaseStatement
          '  EvaluateSelectCaseStatement(CType(s, BoundSelectCaseStatement)) : index += 1
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
          Case Else
            Throw New Exception($"Unexpected kind {s.Kind}")
        End Select
      End While

      Return m_lastValue
    End Function

    'Private Sub EvaluateStatement(node As BoundStatement)
    '  Select Case node.Kind
    '    'Case BoundNodeKind.BlockStatement : EvaluateBlockStatement(CType(node, BoundBlockStatement))
    '    Case BoundNodeKind.VariableDeclaration : EvaluateVariableDeclaration(CType(node, BoundVariableDeclaration))
    '    'Case BoundNodeKind.IfStatement : EvaluateIfStatement(CType(node, BoundIfStatement))
    '    'Case BoundNodeKind.WhileStatement : EvaluateWhileStatement(CType(node, BoundWhileStatement))
    '    'Case BoundNodeKind.ForStatement : EvaluateForStatement(CType(node, BoundForStatement))
    '    'Case BoundNodeKind.SelectCaseStatement : EvaluateSelectCaseStatement(CType(node, BoundSelectCaseStatement))
    '    Case BoundNodeKind.ExpressionStatement : EvaluateExpressionStatement(CType(node, BoundExpressionStatement))
    '    Case Else
    '      Throw New Exception($"Unexpected kind {node.Kind}")
    '  End Select
    'End Sub

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value = EvaluateExpression(node.Initializer)
      'm_variables(node.Variable) = value
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    'Private Sub EvaluateBlockStatement(node As BoundBlockStatement)
    '  For Each statement In node.Statements
    '    EvaluateStatement(statement)
    '  Next
    'End Sub

    'Private Sub EvaluateIfStatement(node As BoundIfStatement)
    '  Dim condition = CBool(EvaluateExpression(node.Condition))
    '  If condition Then
    '    For Each statement In node.IfStatements
    '      EvaluateStatement(statement)
    '    Next
    '  Else
    '    For Each entry In node.ElseIfStatements
    '      condition = CBool(EvaluateExpression(entry.Condition))
    '      If condition Then
    '        For Each statement In entry.Statements
    '          EvaluateStatement(statement)
    '        Next
    '        Exit Sub
    '      End If
    '    Next
    '    For Each statement In node.ElseStatements
    '      EvaluateStatement(statement)
    '    Next
    '  End If
    'End Sub

    'Private Sub EvaluateSelectCaseStatement(node As BoundSelectCaseStatement)
    '  Dim test = CInt(EvaluateExpression(node.Test))
    '  For Each c In node.Cases
    '    For Each match In c.Matches
    '      Dim matched = False
    '      Dim compare = CInt(EvaluateExpression(match.Expression))
    '      Select Case match.Comparison
    '        Case "="
    '          If match.ExpressionTo Is Nothing Then
    '            If test = compare Then matched = True
    '          Else
    '            Dim max = CInt(EvaluateExpression(match.ExpressionTo))
    '            If test >= compare AndAlso test <= max Then matched = True
    '          End If
    '        Case "<" : If test < compare Then matched = True
    '        Case ">" : If test > compare Then matched = True
    '        Case ">=" : If test >= compare Then matched = True
    '        Case "<=" : If test <= compare Then matched = True
    '        Case "<>" : If test <> compare Then matched = True
    '        Case Else
    '          Stop
    '      End Select
    '      If matched Then
    '        For Each statement In c.Statements
    '          EvaluateStatement(statement)
    '        Next
    '        Return
    '      End If
    '    Next
    '  Next
    '  For Each statement In node.ElseStatements
    '    EvaluateStatement(statement)
    '  Next
    'End Sub

    'Private Sub EvaluateWhileStatement(node As BoundWhileStatement)
    '  While CBool(EvaluateExpression(node.Condition))
    '    EvaluateStatement(node.Body)
    '  End While
    'End Sub

    'Private Sub EvaluateForStatement(node As BoundForStatement)
    '  Dim lowerBound = CInt(EvaluateExpression(node.LowerBound))
    '  Dim upperBound = CInt(EvaluateExpression(node.UpperBound))
    '  Dim stepper = 1
    '  If node.Stepper IsNot Nothing Then stepper = CInt(EvaluateExpression(node.Stepper))
    '  For index = lowerBound To upperBound Step stepper
    '    m_variables(node.Variable) = index
    '    EvaluateStatement(node.Body)
    '  Next
    'End Sub

    Private Sub EvaluateExpressionStatement(node As BoundExpressionStatement)
      m_lastValue = EvaluateExpression(node.Expression)
    End Sub

    Private Function EvaluateExpression(node As BoundExpression) As Object
      Select Case node.Kind
        Case BoundNodeKind.LiteralExpression : Return EvaluateLiteralExpression(CType(node, BoundLiteralExpression))
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

    Private Shared Function EvaluateLiteralExpression(node As BoundLiteralExpression) As Object
      Return node.Value
    End Function

    Private Function EvaluateVariableExpression(node As BoundVariableExpression) As Object
      'Return m_variables(node.Variable)
      If node.Variable.Kind = SymbolKind.GlobalVariable Then
        Return m_globals(node.Variable)
      Else
        Dim locals = m_locals.Peek
        Return locals(node.Variable)
      End If
    End Function

    Private Function EvaluateAssignmentExpression(node As BoundAssignmentExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      'm_variables(node.Variable) = value
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

        Case BoundBinaryOperatorKind.BitwiseAnd : Return CInt(left) And CInt(right)
        Case BoundBinaryOperatorKind.LogicalAndAlso : Return CBool(left) AndAlso CBool(right)
        Case BoundBinaryOperatorKind.LogicalOr : Return CBool(left) Or CBool(right)
        Case BoundBinaryOperatorKind.BitwiseOr : Return CInt(left) Or CInt(right)
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
      ElseIf node.[Function] Is BuiltinFunctions.Print Then
        Dim message = CStr(EvaluateExpression(node.Arguments(0)))
        Console.WriteLine(message)
        Return Nothing
      ElseIf node.[Function] Is BuiltinFunctions.Rnd Then
        Dim max = CInt(CLng(Fix(EvaluateExpression(node.Arguments(0)))) Mod Integer.MaxValue)
        If m_random Is Nothing Then m_random = New Random
        Return m_random.[Next](max)
      Else
        'Throw New Exception($"Unexpected function {node.[Function]}")
        Dim locals = New Dictionary(Of VariableSymbol, Object)
        For i = 0 To node.Arguments.Length - 1
          Dim parameter = node.Function.Parameters(i)
          Dim value = EvaluateExpression(node.Arguments(i))
          locals.Add(parameter, value)
        Next
        m_locals.Push(locals)
        Dim statement = m_program.Functions(node.Function)
        Dim result = EvaluateStatement(statement)
        m_locals.Pop()
        Return result
      End If
    End Function

    Private Function EvaluateConversionExpression(node As BoundConversionExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      If node.Type Is TypeSymbol.Boolean Then
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

  ' This Evaluator walks the "raw" SyntaxTree.
  Public NotInheritable Class Evaluator_SyntaxTree

    Private ReadOnly m_root As ExpressionSyntax

    Sub New(root As ExpressionSyntax)
      m_root = root
    End Sub

    Public Function Evaluate() As Integer
      Return EvaluateExpression(m_root)
    End Function

    Private Function EvaluateExpression(node As ExpressionSyntax) As Integer

      If TypeOf node Is LiteralExpressionSyntax Then
        Return CInt(CType(node, LiteralExpressionSyntax).LiteralToken.Value)
      End If

      If TypeOf node Is UnaryExpressionSyntax Then
        Dim u = CType(node, UnaryExpressionSyntax)
        Dim operand = EvaluateExpression(u.Operand)
        Select Case u.OperatorToken.Kind
          Case SyntaxKind.PlusToken : Return operand
          Case SyntaxKind.MinusToken : Return -operand
          Case Else
            Throw New Exception($"Unexpected unary operator {u.OperatorToken.Kind}")
        End Select
      End If

      If TypeOf node Is BinaryExpressionSyntax Then
        Dim b = CType(node, BinaryExpressionSyntax)
        Dim left = EvaluateExpression(b.Left)
        Dim right = EvaluateExpression(b.Right)
        Select Case b.OperatorToken.Kind
          Case SyntaxKind.PlusToken : Return left + right
          Case SyntaxKind.MinusToken : Return left - right
          Case SyntaxKind.StarToken : Return left * right
          Case SyntaxKind.SlashToken : Return CInt(left / right)
          Case SyntaxKind.BackslashToken : Return left \ right
          Case Else
            Throw New Exception($"Unexpected binary operator {b.OperatorToken.Kind}")
        End Select
      End If

      If TypeOf node Is ParenthesizedExpressionSyntax Then
        Dim p = CType(node, ParenthesizedExpressionSyntax)
        Return EvaluateExpression(p.Expression)
      End If

      Throw New Exception($"Unexpected node {node.Kind}")

    End Function

  End Class

End Namespace
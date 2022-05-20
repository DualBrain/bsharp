Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis

  ' This Evaluator utilizes the "Bound Tree" (very similar to the AST).
  Friend NotInheritable Class Evaluator

    Private ReadOnly m_root As BoundStatement
    Private ReadOnly m_variables As Dictionary(Of VariableSymbol, Object)

    Private m_lastValue As Object

    Sub New(root As BoundStatement, variables As Dictionary(Of VariableSymbol, Object))
      m_root = root
      m_variables = variables
    End Sub

    Public Function Evaluate() As Object
      EvaluateStatement(m_root)
      Return m_lastValue
    End Function

    Private Sub EvaluateStatement(node As BoundStatement)
      Select Case node.Kind
        Case BoundNodeKind.BlockStatement : EvaluateBlockStatement(CType(node, BoundBlockStatement))
        Case BoundNodeKind.VariableDeclaration : EvaluateVariableDeclaration(CType(node, BoundVariableDeclaration))
        Case BoundNodeKind.IfStatement : EvaluateIfStatement(CType(node, BoundIfStatement))
        Case BoundNodeKind.WhileStatement : EvaluateWhileStatement(CType(node, BoundWhileStatement))
        Case BoundNodeKind.ForStatement : EvaluateForStatement(CType(node, BoundForStatement))
        Case BoundNodeKind.ExpressionStatement : EvaluateExpressionStatement(CType(node, BoundExpressionStatement))
        Case Else
          Throw New Exception($"Unexpected kind {node.Kind}")
      End Select
    End Sub

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value = EvaluateExpression(node.Initializer)
      m_variables(node.Variable) = value
      m_lastValue = value
    End Sub

    Private Sub EvaluateBlockStatement(node As BoundBlockStatement)
      For Each statement In node.Statements
        EvaluateStatement(statement)
      Next
    End Sub

    Private Sub EvaluateIfStatement(node As BoundIfStatement)
      Dim condition = CBool(EvaluateExpression(node.Condition))
      If condition Then
        If node.ThenStatement IsNot Nothing Then
          EvaluateStatement(node.ThenStatement)
        End If
      ElseIf node.ElseStatement IsNot Nothing Then
        EvaluateStatement(node.ElseStatement)
      End If
    End Sub

    Private Sub EvaluateWhileStatement(node As BoundWhileStatement)
      While CBool(EvaluateExpression(node.Condition))
        EvaluateStatement(node.Body)
      End While
    End Sub

    Private Sub EvaluateForStatement(node As BoundForStatement)
      Dim lowerBound = CInt(EvaluateExpression(node.LowerBound))
      Dim upperBound = CInt(EvaluateExpression(node.UpperBound))
      Dim stepper = 1
      If node.Stepper IsNot Nothing Then stepper = CInt(EvaluateExpression(node.Stepper))
      For index = lowerBound To upperBound Step stepper
        m_variables(node.Variable) = index
        EvaluateStatement(node.Body)
      Next
    End Sub

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
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select
    End Function

    Private Shared Function EvaluateLiteralExpression(node As BoundLiteralExpression) As Object
      Return node.Value
    End Function

    Private Function EvaluateVariableExpression(node As BoundVariableExpression) As Object
      Return m_variables(node.Variable)
    End Function

    Private Function EvaluateAssignmentExpression(node As BoundAssignmentExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      m_variables(node.Variable) = value
      Return value
    End Function

    Private Function EvaluateUnaryExpression(node As BoundUnaryExpression) As Object
      Dim operand = EvaluateExpression(node.Operand)
      Select Case node.Op.Kind
        Case BoundUnaryOperatorKind.Identity : Return CInt(operand)
        Case BoundUnaryOperatorKind.Negation : Return -CInt(operand)
        Case BoundUnaryOperatorKind.LogicalNegation : Return Not CBool(operand)
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
        Case BoundBinaryOperatorKind.Addition : Return CInt(left) + CInt(right)
        Case BoundBinaryOperatorKind.Subtraction : Return CInt(left) - CInt(right)

        Case BoundBinaryOperatorKind.Equal : Return Equals(left, right)
        Case BoundBinaryOperatorKind.GreaterThan : Return CInt(left) > CInt(right)
        Case BoundBinaryOperatorKind.GreaterThanEqual : Return CInt(left) >= CInt(right)
        Case BoundBinaryOperatorKind.LessThan : Return CInt(left) < CInt(right)
        Case BoundBinaryOperatorKind.LessThanEqual : Return CInt(left) <= CInt(right)
        Case BoundBinaryOperatorKind.NotEqual : Return Not Equals(left, right)

        Case BoundBinaryOperatorKind.LogicalAnd : Return CBool(left) And CBool(right)
        Case BoundBinaryOperatorKind.AndOperation : Return CInt(left) And CInt(right)
        Case BoundBinaryOperatorKind.LogicalAndAlso : Return CBool(left) AndAlso CBool(right)
        Case BoundBinaryOperatorKind.LogicalOr : Return CBool(left) Or CBool(right)
        Case BoundBinaryOperatorKind.OrOperation : Return CInt(left) Or CInt(right)
        Case BoundBinaryOperatorKind.LogicalOrElse : Return CBool(left) OrElse CBool(right)

        Case BoundBinaryOperatorKind.XorOperation : Return CInt(left) Xor CInt(right)
        Case BoundBinaryOperatorKind.EqvOperation : Return CBool(left) = CBool(right)
        Case BoundBinaryOperatorKind.ImpOperation : Return CBool(left) AndAlso Not CBool(right)

        Case Else
          Throw New Exception($"Unexpected binary operator {node.Op.Kind}")
      End Select
    End Function

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
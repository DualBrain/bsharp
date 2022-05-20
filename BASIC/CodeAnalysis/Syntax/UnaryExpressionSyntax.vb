Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class UnaryExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(operatorToken As SyntaxToken, operand As ExpressionSyntax)
      Me.OperatorToken = operatorToken
      Me.Operand = operand
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.UnaryExpression
    Public ReadOnly Property OperatorToken As SyntaxToken
    Public ReadOnly Property Operand As ExpressionSyntax

  End Class

End Namespace
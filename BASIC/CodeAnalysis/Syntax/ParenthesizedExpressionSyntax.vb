Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ParenthesizedExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(openParenToken As SyntaxToken, expression As ExpressionSyntax, closeParenToken As SyntaxToken)
      Me.OpenParenToken = openParenToken
      Me.Expression = expression
      Me.CloseParenToken = closeParenToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ParenthesizedExpression
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken

  End Class

End Namespace
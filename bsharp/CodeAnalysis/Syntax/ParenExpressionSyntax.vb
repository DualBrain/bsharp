Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class ParenExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree, openParenToken As SyntaxToken, expression As ExpressionSyntax, closeParenToken As SyntaxToken)
      MyBase.New(tree)
      Me.OpenParenToken = openParenToken
      Me.Expression = expression
      Me.CloseParenToken = closeParenToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ParenExpression
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property CloseParenToken As SyntaxToken

  End Class

End Namespace
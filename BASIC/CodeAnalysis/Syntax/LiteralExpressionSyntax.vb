Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class LiteralExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(literalToken As SyntaxToken)
      Me.New(literalToken, literalToken.Value)
    End Sub

    Sub New(literalToken As SyntaxToken, value As Object)
      Me.LiteralToken = literalToken
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LiteralExpression
    Public ReadOnly Property LiteralToken As SyntaxToken
    Public ReadOnly Property Value As Object

  End Class

End Namespace
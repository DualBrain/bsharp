Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class AssignmentExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(identifierToken As SyntaxToken, equalsToken As SyntaxToken, expression As ExpressionSyntax)
      Me.IdentifierToken = identifierToken
      Me.EqualsToken = equalsToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AssignmentExpression
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property EqualsToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
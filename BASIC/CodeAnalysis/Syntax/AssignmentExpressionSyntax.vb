Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AssignmentExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, letKeyword As SyntaxToken, identifierToken As SyntaxToken, equalsToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.LetKeyword = letKeyword
      Me.IdentifierToken = identifierToken
      Me.EqualToken = equalsToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AssignmentExpression
    Public ReadOnly Property LetKeyword As SyntaxToken
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
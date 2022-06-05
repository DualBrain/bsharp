Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class WhileClauseSyntax
    Inherits SyntaxNode

    Public Sub New(whileKeyword As SyntaxToken, expression As ExpressionSyntax, atBeginning As Boolean)
      Me.WhileKeyword = whileKeyword
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WhileClause
    Public ReadOnly Property WhileKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property AtBeginning As Boolean

  End Class

End Namespace

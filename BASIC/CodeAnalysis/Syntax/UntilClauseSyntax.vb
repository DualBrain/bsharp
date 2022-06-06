Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class UntilClauseSyntax
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree, untilKeyword As SyntaxToken, expression As ExpressionSyntax, atBeginning As Boolean)
      MyBase.New(tree)
      Me.UntilKeyword = untilKeyword
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.UntilClause
    Public ReadOnly Property UntilKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property AtBeginning As Boolean

  End Class

End Namespace

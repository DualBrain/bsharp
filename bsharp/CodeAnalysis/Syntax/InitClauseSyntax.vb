Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class InitClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, equalToken As SyntaxToken, initializer As ExpressionSyntax)
      MyBase.New(tree)
      Me.EqualToken = equalToken
      Me.Initializer = initializer
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.InitClause
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Initializer As ExpressionSyntax
  End Class

End Namespace
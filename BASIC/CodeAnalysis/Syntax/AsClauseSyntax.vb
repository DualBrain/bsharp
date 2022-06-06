Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AsClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, asKeyword As SyntaxToken, identifier As SyntaxToken)
      MyBase.New(tree)
      Me.AsKeyword = asKeyword
      Me.Identifier = identifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AsClause
    Public ReadOnly Property AsKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken

  End Class

End Namespace
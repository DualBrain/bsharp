Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AsClauseSyntax
    Inherits SyntaxNode

    Sub New(asKeyword As SyntaxToken, identifier As SyntaxToken)
      Me.AsKeyword = asKeyword
      Me.Identifier = identifier
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.TypeClause
    Public ReadOnly Property AsKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken

  End Class

End Namespace
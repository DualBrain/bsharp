Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class ParameterSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, identifier As SyntaxToken, asClause As AsClauseSyntax)
      MyBase.New(tree)
      Me.Identifier = identifier
      Me.AsClause = asClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.Parameter
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property AsClause As AsClauseSyntax

  End Class

End Namespace
Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class VariableDeclarationSyntax
    Inherits StatementSyntax

    ' const x = 10
    ' dim x = 10
    ' let x = 10

    Public Sub New(tree As SyntaxTree, keywordToken As SyntaxToken, identifier As SyntaxToken, asClause As AsClauseSyntax, equalToken As SyntaxToken, initializer As ExpressionSyntax)
      MyBase.New(tree)
      Me.Keyword = keywordToken
      Me.Identifier = identifier
      Me.AsClause = asClause
      Me.EqualToken = equalToken
      Me.Initializer = initializer
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.VariableDeclaration
    Public ReadOnly Property Keyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property AsClause As AsClauseSyntax
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Initializer As ExpressionSyntax

  End Class

End Namespace
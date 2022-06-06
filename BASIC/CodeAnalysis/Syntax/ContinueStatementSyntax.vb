Namespace Basic.CodeAnalysis.Syntax

  Partial Friend Class ContinueStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, continueKeyword As SyntaxToken, scopeKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ContinueKeyword = continueKeyword
      Me.ScopeKeyword = scopeKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ContinueStatement
    Public ReadOnly Property ContinueKeyword() As SyntaxToken
    Public ReadOnly Property ScopeKeyword As SyntaxToken

  End Class

End Namespace
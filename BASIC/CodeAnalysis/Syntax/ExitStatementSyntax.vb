Namespace Basic.CodeAnalysis.Syntax

  Partial Friend Class ExitStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, exitKeyword As SyntaxToken, scopeKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ExitKeyword = exitKeyword
      Me.ScopeKeyword = scopeKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ExitStatement
    Public ReadOnly Property ExitKeyword As SyntaxToken
    Public ReadOnly Property ScopeKeyword As SyntaxToken

  End Class

End Namespace
Namespace Basic.CodeAnalysis.Syntax

  Partial Friend Class ExitStatementSyntax
    Inherits StatementSyntax

    'tree As SyntaxTree, 
    Public Sub New(exitKeyword As SyntaxToken, scopeKeyword As SyntaxToken)
      'MyBase.New(tree)
      Me.ExitKeyword = exitKeyword
      Me.ScopeKeyword = scopeKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ExitStatement
    Public ReadOnly Property ExitKeyword As SyntaxToken
    Public ReadOnly Property ScopeKeyword As SyntaxToken

  End Class

End Namespace
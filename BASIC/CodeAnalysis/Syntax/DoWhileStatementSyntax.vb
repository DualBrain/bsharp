Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class DoWhileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(doKeyword As SyntaxToken, whileClause As WhileClauseSyntax, body As StatementSyntax, loopKeyword As SyntaxToken)
      Me.DoKeyword = doKeyword
      Me.WhileClause = whileClause
      Me.Body = body
      Me.LoopKeyword = loopKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DoWhileStatement
    Public ReadOnly Property DoKeyword As SyntaxToken
    Public ReadOnly Property WhileClause As WhileClauseSyntax
    Public ReadOnly Property Body As StatementSyntax
    Public ReadOnly Property LoopKeyword As SyntaxToken

  End Class

End Namespace
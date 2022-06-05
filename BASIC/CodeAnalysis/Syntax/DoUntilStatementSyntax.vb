Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class DoUntilStatementSyntax
    Inherits StatementSyntax

    Public Sub New(doKeyword As SyntaxToken, untilClause As UntilClauseSyntax, body As StatementSyntax, loopKeyword As SyntaxToken)
      Me.DoKeyword = doKeyword
      Me.UntilClause = untilClause
      Me.Body = body
      Me.LoopKeyword = loopKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DoUntilStatement
    Public ReadOnly Property DoKeyword As SyntaxToken
    Public ReadOnly Property UntilClause As UntilClauseSyntax
    Public ReadOnly Property Body As StatementSyntax
    Public ReadOnly Property LoopKeyword As SyntaxToken

  End Class

End Namespace
Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class DoUntilStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, doKeyword As SyntaxToken, untilClause As UntilClauseSyntax, body As StatementSyntax, loopKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.DoKeyword = doKeyword
      Me.UntilClause = untilClause
      Me.Statements = body
      Me.LoopKeyword = loopKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DoUntilStatement
    Public ReadOnly Property DoKeyword As SyntaxToken
    Public ReadOnly Property UntilClause As UntilClauseSyntax
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property LoopKeyword As SyntaxToken

  End Class

End Namespace
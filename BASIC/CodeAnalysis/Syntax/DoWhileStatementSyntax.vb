Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class DoWhileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, doKeyword As SyntaxToken, whileClause As WhileClauseSyntax, body As StatementSyntax, loopKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.DoKeyword = doKeyword
      Me.WhileClause = whileClause
      Me.Statements = body
      Me.LoopKeyword = loopKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DoWhileStatement
    Public ReadOnly Property DoKeyword As SyntaxToken
    Public ReadOnly Property WhileClause As WhileClauseSyntax
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property LoopKeyword As SyntaxToken

  End Class

End Namespace
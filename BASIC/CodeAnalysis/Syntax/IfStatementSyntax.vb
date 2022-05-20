Namespace Basic.CodeAnalysis.Syntax
  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(ifKeyword As SyntaxToken,
                   condition As ExpressionSyntax,
                   thenStatement As StatementSyntax,
                   elseClause As ElseClauseSyntax)
      Me.IfKeyword = ifKeyword
      Me.Condition = condition
      Me.ThenStatement = thenStatement
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property ThenStatement As StatementSyntax
    Public ReadOnly Property ElseClause As ElseClauseSyntax
  End Class

  Public NotInheritable Class ElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(elseKeyword As SyntaxToken, elseStatement As StatementSyntax)
      Me.ElseKeyword = elseKeyword
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseClause
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property ElseStatement As StatementSyntax
  End Class

End Namespace
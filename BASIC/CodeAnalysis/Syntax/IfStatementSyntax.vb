Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax
  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(ifKeyword As SyntaxToken,
                   condition As ExpressionSyntax,
                   thenStatements As ImmutableArray(Of StatementSyntax),
                   elseClause As ElseClauseSyntax)
      Me.IfKeyword = ifKeyword
      Me.Condition = condition
      Me.ThenStatements = thenStatements
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property ThenStatements As ImmutableArray(Of StatementSyntax)
    Public ReadOnly Property ElseClause As ElseClauseSyntax
  End Class

  Public NotInheritable Class ElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(elseKeyword As SyntaxToken, elseStatements As ImmutableArray(Of StatementSyntax))
      Me.ElseKeyword = elseKeyword
      Me.ElseStatements = elseStatements
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseClause
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property ElseStatements As ImmutableArray(Of StatementSyntax)
  End Class

End Namespace
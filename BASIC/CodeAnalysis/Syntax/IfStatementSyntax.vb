Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax
  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(condition As ExpressionSyntax,
                   trueBlock As ImmutableArray(Of StatementSyntax),
                   elseIfClauses As ImmutableArray(Of ElseIfClauseSyntax),
                   elseClause As ElseClauseSyntax)
      Me.Condition = condition
      Me.TrueBlock = trueBlock
      Me.ElseIfClauses = elseIfClauses
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property TrueBlock As ImmutableArray(Of StatementSyntax)
    Public ReadOnly Property ElseIfClauses As ImmutableArray(Of ElseIfClauseSyntax)
    Public ReadOnly Property ElseClause As ElseClauseSyntax

  End Class

  Public NotInheritable Class ElseIfClauseSyntax
    Inherits SyntaxNode

    Public Sub New(condition As ExpressionSyntax, statements As ImmutableArray(Of StatementSyntax))
      Me.Condition = condition
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseIfClause
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property Statements As ImmutableArray(Of StatementSyntax)

  End Class

  Public NotInheritable Class ElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(elseBlock As ImmutableArray(Of StatementSyntax))
      Me.ElseBlock = elseBlock
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseClause
    Public ReadOnly Property ElseBlock As ImmutableArray(Of StatementSyntax)

  End Class

End Namespace
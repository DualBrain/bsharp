Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax
  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(condition As ExpressionSyntax,
                   trueBlock As ImmutableArray(Of StatementSyntax),
                   additionalIfClauses As ImmutableArray(Of IfClauseSyntax),
                   elseClause As ElseClauseSyntax)
      Me.Condition = condition
      Me.TrueBlock = trueBlock
      Me.AdditionalIfClauses = additionalIfClauses
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property TrueBlock As ImmutableArray(Of StatementSyntax)
    Public ReadOnly Property AdditionalIfClauses As ImmutableArray(Of IfClauseSyntax)
    Public ReadOnly Property ElseClause As ElseClauseSyntax

  End Class

  Public NotInheritable Class IfClauseSyntax
    Inherits SyntaxNode

    Public Sub New(condition As ExpressionSyntax, trueBlock As ImmutableArray(Of StatementSyntax))
      Me.Condition = condition
      Me.TrueBlock = trueBlock
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseClause
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property TrueBlock As ImmutableArray(Of StatementSyntax)

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
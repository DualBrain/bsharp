Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSelectCaseStatement
    Inherits BoundStatement

    Public Sub New(test As BoundExpression, cases As ImmutableArray(Of BoundCaseStatement), elseStatements As ImmutableArray(Of BoundStatement))
      Me.Test = test
      Me.Cases = cases
      Me.ElseStatements = elseStatements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SelectCaseStatement
    Public ReadOnly Property Test As BoundExpression
    Public ReadOnly Property Cases As ImmutableArray(Of BoundCaseStatement)
    Public ReadOnly Property ElseStatements As ImmutableArray(Of BoundStatement)

  End Class

  Friend NotInheritable Class BoundCaseStatement
    Inherits BoundStatement

    Public Sub New(matches As ImmutableArray(Of BoundMatchStatement), statements As ImmutableArray(Of BoundStatement))
      Me.Matches = matches
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BoundCaseStatement
    Public ReadOnly Property Matches As ImmutableArray(Of BoundMatchStatement)
    Public ReadOnly Property Statements As ImmutableArray(Of BoundStatement)

  End Class

  Friend NotInheritable Class BoundMatchStatement
    Inherits BoundStatement

    Public Sub New(comparison As String, expression As BoundExpression, expressionTo As BoundExpression)
      Me.Comparison = comparison
      Me.Expression = expression
      Me.ExpressionTo = expressionTo
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BoundMatchStatement
    Public ReadOnly Property Comparison As String
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property ExpressionTo As BoundExpression

  End Class

End Namespace

Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundIfStatement
    Inherits BoundStatement

    Public Sub New(condition As BoundExpression, ifStatements As ImmutableArray(Of BoundStatement), elseIfStatements As ImmutableArray(Of BoundElseIfStatement), elseStatements As ImmutableArray(Of BoundStatement))
      Me.Condition = condition
      Me.IfStatements = ifStatements
      Me.ElseIfStatements = elseIfStatements
      Me.ElseStatements = elseStatements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.IfStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property IfStatements As ImmutableArray(Of BoundStatement)
    Public ReadOnly Property ElseIfStatements As ImmutableArray(Of BoundElseIfStatement)
    Public ReadOnly Property ElseStatements As ImmutableArray(Of BoundStatement)

  End Class

  Friend NotInheritable Class BoundElseIfStatement
    Inherits BoundStatement

    Public Sub New(condition As BoundExpression, statements As ImmutableArray(Of BoundStatement))
      Me.Condition = condition
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ElseIfStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property Statements As ImmutableArray(Of BoundStatement)

  End Class

End Namespace

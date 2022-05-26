Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundIfStatement
    Inherits BoundStatement

    Public Sub New(condition As BoundExpression, thenStatements As ImmutableArray(Of BoundStatement), elseStatements As ImmutableArray(Of BoundStatement))
      Me.Condition = condition
      Me.ThenStatements = thenStatements
      Me.ElseStatements = elseStatements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.IfStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property ThenStatements As ImmutableArray(Of BoundStatement)
    Public ReadOnly Property ElseStatements As ImmutableArray(Of BoundStatement)

  End Class

End Namespace

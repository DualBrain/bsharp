Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundIfStatement
    Inherits BoundStatement

    Public Sub New(condition As BoundExpression, ifStatement As BoundStatement, elseStatement As BoundStatement)
      Me.Condition = condition
      Me.IfStatement = ifStatement
      'Me.ElseIfStatements = ElseIfStatements
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.IfStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property IfStatement As BoundStatement
    'Public ReadOnly Property ElseIfStatements As ImmutableArray(Of BoundElseIfStatement)
    Public ReadOnly Property ElseStatement As BoundStatement

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

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDoWhileStatement
    Inherits BoundStatement

    Public Sub New(body As BoundStatement, condition As BoundExpression)
      Me.Body = body
      Me.Condition = condition
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoWhileStatement
    Public ReadOnly Property Body As BoundStatement
    Public ReadOnly Property Condition As BoundExpression

  End Class

End Namespace
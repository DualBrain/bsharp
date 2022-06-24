Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundTabFunction
    Inherits BoundStatement

    Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.TabFunction
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
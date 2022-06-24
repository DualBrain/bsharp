Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSpcFunction
    Inherits BoundStatement

    Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SpcFunction
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
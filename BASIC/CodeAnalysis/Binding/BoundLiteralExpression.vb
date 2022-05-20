Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLiteralExpression
    Inherits BoundExpression

    Sub New(value As Object)
      Me.Value = value
      Me.Type = value.GetType
    End Sub

    Public Overrides ReadOnly Property Type As Type ' = Value.GetType
    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LiteralExpression
    Public ReadOnly Property Value As Object
  End Class

End Namespace
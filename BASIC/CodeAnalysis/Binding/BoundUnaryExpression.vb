Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryExpression
    Inherits BoundExpression

    Public Sub New(op As BoundUnaryOperator, operand As BoundExpression)
      Me.Op = op
      Me.Operand = operand
      Me.Type = op.Type
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.UnaryExpression
    Public Overrides ReadOnly Property Type As Type '= Operand.Type
    Public ReadOnly Property Op As BoundUnaryOperator
    Public ReadOnly Property Operand As BoundExpression

  End Class

End Namespace
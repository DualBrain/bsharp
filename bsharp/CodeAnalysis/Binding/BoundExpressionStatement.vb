Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundExpressionStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ExpressionStatement
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
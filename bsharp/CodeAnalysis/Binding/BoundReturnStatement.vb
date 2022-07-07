Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundReturnStatement
    Inherits BoundStatement

    Public Sub New()
      Me.Expression = Nothing
    End Sub

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind() As BoundNodeKind = BoundNodeKind.ReturnStatement
    Public ReadOnly Property Expression() As BoundExpression

  End Class

End Namespace
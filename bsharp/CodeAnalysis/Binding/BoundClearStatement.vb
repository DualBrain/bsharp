Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundClearStatement
    Inherits BoundStatement

    Public Sub New(maxBytesExpression As BoundExpression, stackSpaceExpression As BoundExpression)
      Me.MaxBytesExpression = maxBytesExpression
      Me.StackSpaceExpression = stackSpaceExpression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ClearStatement
    Public ReadOnly Property MaxBytesExpression As BoundExpression
    Public ReadOnly Property StackSpaceExpression As BoundExpression

  End Class

End Namespace
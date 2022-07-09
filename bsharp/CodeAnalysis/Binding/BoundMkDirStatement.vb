Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundMkDirStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.MkDirStatement
    Public ReadOnly Property Expression As BoundExpression

  End Class

  Friend NotInheritable Class BoundKillStatement
    Inherits BoundStatement

    Public Sub New(expression As BoundExpression)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.KillStatement
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
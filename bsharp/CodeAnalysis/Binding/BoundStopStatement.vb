Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundStopStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.StopStatement

  End Class

End Namespace
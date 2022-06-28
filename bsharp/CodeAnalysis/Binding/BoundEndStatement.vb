Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundEndStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.EndStatement

  End Class

End Namespace
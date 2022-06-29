Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSystemStatement
    Inherits BoundStatement

    Public Sub New()
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SystemStatement

  End Class

End Namespace
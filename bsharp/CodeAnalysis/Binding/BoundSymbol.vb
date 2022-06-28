Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundSymbol
    Inherits BoundNode

    Friend Sub New(value As String)
      Me.Value = value
    End Sub

    Public ReadOnly Property Value As String

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.Symbol

    Public Overrides Function ToString() As String
      Return Value
    End Function

  End Class

End Namespace
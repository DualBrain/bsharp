Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOptionStatement
    Inherits BoundStatement

    Public Sub New(number As Integer)
      Me.Number = number
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OptionStatement
    Public ReadOnly Property Number As Integer

  End Class

End Namespace
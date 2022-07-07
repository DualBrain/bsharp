Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundGosubStatement
    Inherits BoundStatement

    Sub New(label As BoundLabel)
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.GosubStatement
    Public ReadOnly Property Label As BoundLabel

  End Class

End Namespace
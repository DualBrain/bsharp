﻿Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLabelStatement
    Inherits BoundStatement

    Sub New(label As BoundLabel)
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LabelStatement
    Public ReadOnly Property Label As BoundLabel

  End Class

End Namespace
﻿Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundReturnGosubStatement
    Inherits BoundStatement

    Sub New(label As BoundLabel)
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ReturnGosubStatement
    Public ReadOnly Property Label As BoundLabel

  End Class

End Namespace
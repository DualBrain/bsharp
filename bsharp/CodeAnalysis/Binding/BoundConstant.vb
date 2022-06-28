Option Explicit On
Option Strict On
Option Infer On

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundConstant

    Public Sub New(value As Object)
      Me.Value = value
    End Sub

    Public ReadOnly Property Value As Object

  End Class

End Namespace
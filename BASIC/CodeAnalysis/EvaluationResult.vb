Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis

  Public NotInheritable Class EvaluationResult

    Public Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object)
      Me.Diagnostics = diagnostics
      Me.Value = value
    End Sub

    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Value As Object

  End Class

End Namespace
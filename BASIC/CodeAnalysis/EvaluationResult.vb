Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis

  Public NotInheritable Class EvaluationResult

    Sub New(diagnostics As ImmutableArray(Of Diagnostic), value As Object)
      Me.Diagnostics = diagnostics
      Me.Value = value
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray
    End Sub

    ' TODO: I think we should not have separate collections but instead
    '       have an extension method over ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property ErrorDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property WarningDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Value As Object

  End Class

End Namespace
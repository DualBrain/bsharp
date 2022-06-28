Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices

Namespace Bsharp.CodeAnalysis

  Public Module DiagnosticExtensions

    <Extension()>
    Public Function HasErrors(diagnostics As ImmutableArray(Of Diagnostic)) As Boolean
      Return diagnostics.Any(Function(d) d.IsError)
    End Function

    <Extension()>
    Public Function HasErrors(diagnostics As IEnumerable(Of Diagnostic)) As Boolean
      Return diagnostics.Any(Function(d) d.IsError)
    End Function

  End Module

End Namespace

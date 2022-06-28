Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundProgram

    '
    '
    '
    Public Sub New(previous As BoundProgram,
                   diagnostics As ImmutableArray(Of Diagnostic),
                   mainFunction As FunctionSymbol,
                   scriptFunction As FunctionSymbol,
                   functions As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement))
      Me.Previous = previous
      Me.Diagnostics = diagnostics
      Me.MainFunction = mainFunction
      Me.ScriptFunction = scriptFunction
      Me.Functions = functions
      ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray()
      WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray()
    End Sub

    Public ReadOnly Property Previous As BoundProgram
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property ErrorDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property WarningDiagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property MainFunction As FunctionSymbol
    Public ReadOnly Property ScriptFunction As FunctionSymbol
    Public ReadOnly Property Functions As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement)
    'Public ReadOnly Property Statement As BoundBlockStatement

  End Class

End Namespace
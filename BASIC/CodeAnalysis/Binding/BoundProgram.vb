Imports System.Collections.Immutable
Imports System.Linq
Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundProgram

    'previous As BoundProgram,
    'mainFunction As FunctionSymbol,
    'scriptFunction As FunctionSymbol,
    Public Sub New(diagnostics As ImmutableArray(Of Diagnostic),
                   functions As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement),
                   statement As BoundBlockStatement)
      'Me.Previous = Previous
      Me.Diagnostics = diagnostics
      'Me.MainFunction = MainFunction
      'Me.ScriptFunction = ScriptFunction
      Me.Functions = functions
      Me.Statement = statement
      'ErrorDiagnostics = diagnostics.Where(Function(d) d.IsError).ToImmutableArray()
      'WarningDiagnostics = diagnostics.Where(Function(d) d.IsWarning).ToImmutableArray()
    End Sub

    'Public ReadOnly Property Previous As BoundProgram
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    'Public ReadOnly Property ErrorDiagnostics As ImmutableArray(Of Diagnostic)
    'Public ReadOnly Property WarningDiagnostics As ImmutableArray(Of Diagnostic)
    'Public ReadOnly Property MainFunction As FunctionSymbol
    'Public ReadOnly Property ScriptFunction As FunctionSymbol
    Public ReadOnly Property Functions As ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement)
    Public ReadOnly Property Statement As BoundBlockStatement
    'Public ReadOnly Property Statement As BoundBlockStatement
  End Class

End Namespace
Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding
  Friend NotInheritable Class BoundGlobalScope

    Public Sub New(previous As BoundGlobalScope, diagnostics As ImmutableArray(Of Diagnostic), variables As ImmutableArray(Of VariableSymbol), statement As BoundStatement)
      Me.Previous = previous
      Me.Diagnostics = diagnostics
      Me.Variables = variables
      Me.Statement = statement
    End Sub

    Public ReadOnly Property Previous As BoundGlobalScope
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)
    Public ReadOnly Property Statement As BoundStatement

  End Class

End Namespace
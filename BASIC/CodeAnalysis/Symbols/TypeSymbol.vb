Namespace Basic.CodeAnalysis.Symbols

  Public NotInheritable Class TypeSymbol
    Inherits Symbol

    Public Shared ReadOnly [Error] As New TypeSymbol("?")
    Public Shared ReadOnly Any As New TypeSymbol("any")
    Public Shared ReadOnly [Boolean] As New TypeSymbol("boolean")
    Public Shared ReadOnly [Integer] As New TypeSymbol("integer")
    Public Shared ReadOnly [String] As New TypeSymbol("string")
    Public Shared ReadOnly [Nothing] As New TypeSymbol("nothing")

    Private Sub New(name As String)
      MyBase.New(name)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Type

  End Class

End Namespace
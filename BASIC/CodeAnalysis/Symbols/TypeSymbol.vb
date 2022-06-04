Namespace Basic.CodeAnalysis.Symbols

  Public NotInheritable Class TypeSymbol
    Inherits Symbol

    Public Shared ReadOnly [Error] As New TypeSymbol("?")
    Public Shared ReadOnly Any As New TypeSymbol("any")
    Public Shared ReadOnly Bool As New TypeSymbol("bool")
    Public Shared ReadOnly Int As New TypeSymbol("int")
    Public Shared ReadOnly [String] As New TypeSymbol("string")
    Public Shared ReadOnly Void As New TypeSymbol("void")

    Private Sub New(name As String)
      MyBase.New(name)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Type

  End Class

End Namespace
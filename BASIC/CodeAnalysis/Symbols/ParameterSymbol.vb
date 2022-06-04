Namespace Basic.CodeAnalysis.Symbols

  Public NotInheritable Class ParameterSymbol
    Inherits VariableSymbol 'LocalVariableSymbol

    Sub New(name As String, type As TypeSymbol) ', ordinal As Integer)
      MyBase.New(name, True, type) ', Nothing)
      'Me.Ordinal = Ordinal
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Parameter
    'Public ReadOnly Property Ordinal As Integer

  End Class

End Namespace
Imports Basic.CodeAnalysis.Binding

Namespace Basic.CodeAnalysis.Symbols

  'Public MustInherit Class VariableSymbol
  Public Class VariableSymbol
    Inherits Symbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol) ', constant As BoundConstant)
      MyBase.New(name)
      Me.IsReadOnly = isReadOnly
      Me.Type = type
      'Me.Constant = If(isReadOnly, Constant, Nothing)
    End Sub

    'Public ReadOnly Property Name As String
    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Variable
    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As TypeSymbol

    'Friend ReadOnly Property Constant As BoundConstant

  End Class

End Namespace
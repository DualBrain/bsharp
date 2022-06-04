Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Symbols

  Public NotInheritable Class FunctionSymbol
    Inherits Symbol

    Sub New(name As String, paremeters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol) ', Optional declaration As FunctionDeclarationSyntax = Nothing)
      MyBase.New(name)
      Parameters = paremeters
      Me.Type = type
      'Me.Declaration = Declaration
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Function
    'Public ReadOnly Property Declaration As FunctionDeclarationSyntax
    Public ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
    Public ReadOnly Property Type As TypeSymbol

  End Class

End Namespace
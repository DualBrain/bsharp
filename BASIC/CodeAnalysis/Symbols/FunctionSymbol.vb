Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis.Symbols

  Public NotInheritable Class FunctionSymbol
    Inherits Symbol

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, Optional declaration As FunctionDeclarationSyntax = Nothing)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = declaration
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Function
    Public ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
    Public ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Declaration As FunctionDeclarationSyntax

  End Class

End Namespace
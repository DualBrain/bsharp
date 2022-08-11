Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    Private m_symbols As Dictionary(Of String, Symbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclareVariable(variable As VariableSymbol) As Boolean
      Return TryDeclareSymbol(variable)
    End Function

    Public Function TryDeclareFunction(f As FunctionSymbol) As Boolean
      Return TryDeclareSymbol(f)
    End Function

    Private Function TryDeclareSymbol(Of TSymbol As Symbol)(symbol As TSymbol) As Boolean
      Dim name = symbol.Name
      'If (symbol.Kind = SymbolKind.LocalVariable OrElse
      '    symbol.Kind = SymbolKind.GlobalVariable) AndAlso
      '   Not "%&!#$".Contains(name.Last) Then
      '  'TODO: Need to determine type based on DEF... setting.
      '  '      Until that happens, default to single-precision.
      '  name &= "!"c
      'End If
      Dim key = $"{name.ToLower}"
      If symbol.Kind = SymbolKind.Function Then
        Dim f = TryCast(symbol, FunctionSymbol)
        key &= $"[{If(f?.Parameters.Length, 0)}]"
      End If
      If m_symbols Is Nothing Then
        m_symbols = New Dictionary(Of String, Symbol)
      ElseIf m_symbols.ContainsKey(key) Then
        Return False
      End If
      m_symbols.Add(key, symbol)
      Return True
    End Function

    Public Function TryLookupFunction(name As String, parameters As List(Of TypeSymbol)) As Symbol
      'If Not "%&!#$".Contains(name.Last) Then name &= "!"c
      Dim key = $"{name.ToLower}"
      key &= $"[{If(parameters?.Count, 0)}]"
      Dim result = TryLookupSymbol(key)
      If result Is Nothing Then
        key = $"{name.ToLower}["
        result = TryLookupSymbol(key)
      End If
      Return result
    End Function

    Public Function TryLookupSymbol(name As String) As Symbol
      Dim symbol As Symbols.Symbol = Nothing
      'If Not "%&!#$".Contains(name.Last) Then name &= "!"c
      If m_symbols IsNot Nothing AndAlso m_symbols.TryGetValue(name.ToLower, symbol) Then
        Return symbol
      ElseIf m_symbols IsNot Nothing AndAlso name.EndsWith("[") Then
        Dim result = (From p In m_symbols Where p.Key.StartsWith(name.ToLower) Select p.Value).FirstOrDefault
        If result IsNot Nothing Then Return result
      End If
      Return Parent?.TryLookupSymbol(name)
    End Function

    Public Function GetDeclaredVariables() As ImmutableArray(Of VariableSymbol)
      Return GetDeclaredSymbols(Of VariableSymbol)()
    End Function

    Public ReadOnly Property GetDeclaredFunctions As ImmutableArray(Of FunctionSymbol)
      Get
        Return GetDeclaredSymbols(Of FunctionSymbol)()
      End Get
    End Property

    Private Function GetDeclaredSymbols(Of TSymbol As Symbol)() As ImmutableArray(Of TSymbol)
      If m_symbols Is Nothing Then Return ImmutableArray(Of TSymbol).Empty
      Return m_symbols.Values.OfType(Of TSymbol)().ToImmutableArray()
    End Function

  End Class

End Namespace
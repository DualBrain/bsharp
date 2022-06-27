Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

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
      'TODO: Handle overloading/optional parameters.
      If m_symbols Is Nothing Then
        m_symbols = New Dictionary(Of String, Symbol)
      ElseIf m_symbols.ContainsKey(symbol.Name) Then
        Return False
      End If
      m_symbols.Add(symbol.Name, symbol)
      Return True
    End Function

    Public Function TryLookupSymbol(name As String) As Symbol
      Dim symbol As Symbols.Symbol = Nothing
      'TODO: Handle Function Operator Overloading (duplicate functions names with different parameters/types).
      'TODO: Handle Optional Parameters.
      If m_symbols IsNot Nothing AndAlso m_symbols.TryGetValue(name.ToLower, symbol) Then
        Return symbol
      End If
      Return Parent?.TryLookupSymbol(name)
    End Function

    Public Function GetDeclaredVariables() As ImmutableArray(Of VariableSymbol)
      Return GetDeclaredSymbols(Of VariableSymbol)()
    End Function

    Public ReadOnly Property GetDeclaredFunctions As ImmutableArray(Of FunctionSymbol)
      Get
        Return GetDeclaredSymbols(Of FunctionSymbol)
      End Get
    End Property

    Private Function GetDeclaredSymbols(Of TSymbol As Symbol)() As ImmutableArray(Of TSymbol)
      If m_symbols Is Nothing Then Return ImmutableArray(Of TSymbol).Empty
      Return m_symbols.Values.OfType(Of TSymbol)().ToImmutableArray()
    End Function

  End Class

End Namespace
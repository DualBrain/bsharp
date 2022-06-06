Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    'Private m_variables As Dictionary(Of String, VariableSymbol)
    'Private m_functions As Dictionary(Of String, FunctionSymbol)
    Private m_symbols As Dictionary(Of String, Symbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclareVariable(variable As VariableSymbol) As Boolean
      Return TryDeclareSymbol(variable)
      'If m_variables Is Nothing Then m_variables = New Dictionary(Of String, VariableSymbol)
      'If m_variables.ContainsKey(variable.Name) Then Return False
      'm_variables.Add(variable.Name, variable)
      'Return True
    End Function

    Public Function TryDeclareFunction(f As FunctionSymbol) As Boolean
      Return TryDeclareSymbol(f)
      'If m_functions Is Nothing Then m_functions = New Dictionary(Of String, FunctionSymbol)
      'If m_functions.ContainsKey(f.Name) Then Return False
      'm_functions.Add(f.Name, f)
      'Return True
    End Function

    Private Function TryDeclareSymbol(Of TSymbol As Symbol)(symbol As TSymbol) As Boolean
      If m_symbols Is Nothing Then
        m_symbols = New Dictionary(Of String, Symbol)
      ElseIf m_symbols.ContainsKey(symbol.Name) Then
        Return False
      End If
      m_symbols.Add(symbol.Name, symbol)
      Return True
    End Function

    'Public Function TryLookupVariable(name As String, ByRef variable As VariableSymbol) As Boolean
    '  Return TryLookupSymbol(name, variable)
    '  'If m_variables IsNot Nothing AndAlso m_variables.TryGetValue(name, variable) Then Return True
    '  'If Parent Is Nothing Then Return False
    '  'Return Parent.TryLookupVariable(name, variable)
    'End Function

    'Public Function TryLookupFunction(name As String, ByRef f As FunctionSymbol) As Boolean
    '  Return TryLookupSymbol(name, f)
    '  'f = Nothing
    '  'If m_functions IsNot Nothing AndAlso m_functions.TryGetValue(name, f) Then Return True
    '  'If Parent Is Nothing Then Return False
    '  'Return Parent.TryLookupFunction(name, f)
    'End Function

    'Public Function TryLookupSymbol(Of TSymbol As Symbol)(name As String, ByRef symbol As TSymbol) As Symbol 'Boolean
    Public Function TryLookupSymbol(name As String) As Symbol 'Boolean
      'symbol = Nothing
      'Dim declaredSymbol As Symbol = Nothing
      'If m_symbols IsNot Nothing AndAlso m_symbols.TryGetValue(name, declaredSymbol) Then
      '  If TypeOf declaredSymbol Is TSymbol Then
      '    symbol = DirectCast(declaredSymbol, TSymbol)
      '    Return True
      '  End If
      '  Return False
      'End If
      'If Parent Is Nothing Then Return False
      Dim symbol As Symbols.Symbol = Nothing
      If m_symbols IsNot Nothing AndAlso m_symbols.TryGetValue(name, symbol) Then
        Return symbol
      End If
      'Return Parent.TryLookupSymbol(name, symbol)
      Return Parent?.TryLookupSymbol(name)
    End Function

    Public ReadOnly Property GetDeclaredVariables As ImmutableArray(Of VariableSymbol)
      Get
        Return GetDeclaredSymbols(Of VariableSymbol)
        'If m_variables Is Nothing Then Return ImmutableArray(Of VariableSymbol).Empty
        'Return m_variables.Values.ToImmutableArray
      End Get
    End Property

    Public ReadOnly Property GetDeclaredFunctions As ImmutableArray(Of FunctionSymbol)
      Get
        Return GetDeclaredSymbols(Of FunctionSymbol)
        'If m_functions Is Nothing Then Return ImmutableArray(Of FunctionSymbol).Empty
        'Return m_functions.Values.ToImmutableArray
      End Get
    End Property

    Private Function GetDeclaredSymbols(Of TSymbol As Symbol)() As ImmutableArray(Of TSymbol)
      If m_symbols Is Nothing Then Return ImmutableArray(Of TSymbol).Empty
      Return m_symbols.Values.OfType(Of TSymbol)().ToImmutableArray()
    End Function

  End Class

End Namespace
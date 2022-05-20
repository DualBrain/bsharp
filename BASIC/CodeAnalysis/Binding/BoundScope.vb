Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundScope

    Private ReadOnly m_variables As New Dictionary(Of String, VariableSymbol)

    Public Sub New(parent As BoundScope)
      Me.Parent = parent
    End Sub

    Public ReadOnly Property Parent As BoundScope

    Public Function TryDeclare(variable As VariableSymbol) As Boolean
      If m_variables.ContainsKey(variable.Name) Then Return False
      m_variables.Add(variable.Name, variable)
      Return True
    End Function

    Public Function TryLookup(name As String, ByRef variable As VariableSymbol) As Boolean
      If m_variables.TryGetValue(name, variable) Then Return True
      If Parent Is Nothing Then Return False
      Return Parent.TryLookup(name, variable)
    End Function

    Public ReadOnly Property GetDeclaredVariables As ImmutableArray(Of VariableSymbol)
      Get
        Return m_variables.Values.ToImmutableArray
      End Get
    End Property

  End Class

End Namespace
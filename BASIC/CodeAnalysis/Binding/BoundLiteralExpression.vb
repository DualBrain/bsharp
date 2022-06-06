Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLiteralExpression
    Inherits BoundExpression

    Sub New(value As Object)
      'Me.Value = value
      If TypeOf value Is Boolean Then
        Me.Type = TypeSymbol.Boolean
      ElseIf TypeOf value Is Integer Then
        Me.Type = TypeSymbol.Integer
      ElseIf TypeOf value Is String Then
        Me.Type = TypeSymbol.String
      Else
        Throw New Exception($"Unexpected literal '{value}' of type {value.GetType}.")
      End If
      ConstantValue = New BoundConstant(value)
    End Sub

    Public Overrides ReadOnly Property Type As TypeSymbol
    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.LiteralExpression
    Public ReadOnly Property Value As Object
      Get
        Return ConstantValue.Value
      End Get
    End Property
    Public Overrides ReadOnly Property ConstantValue As BoundConstant

  End Class

End Namespace
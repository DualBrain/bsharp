Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundLiteralExpression
    Inherits BoundExpression

    Sub New(value As Object)
      If TypeOf value Is Decimal Then
        Me.Type = TypeSymbol.Decimal
      ElseIf TypeOf value Is Double Then
        Me.Type = TypeSymbol.Double
      ElseIf TypeOf value Is Single Then
        Me.Type = TypeSymbol.Single
      ElseIf TypeOf value Is ULong Then
        Me.Type = TypeSymbol.ULong64
      ElseIf TypeOf value Is Long Then
        Me.Type = TypeSymbol.Long64
      ElseIf TypeOf value Is UInteger Then
        Me.Type = TypeSymbol.ULong
      ElseIf TypeOf value Is Integer Then
        Me.Type = TypeSymbol.Long
      ElseIf TypeOf value Is UShort Then
        Me.Type = TypeSymbol.UInteger
      ElseIf TypeOf value Is Short Then
        Me.Type = TypeSymbol.Integer
      ElseIf TypeOf value Is Char Then
        Me.Type = TypeSymbol.Char
      ElseIf TypeOf value Is SByte Then
        Me.Type = TypeSymbol.SByte
      ElseIf TypeOf value Is Byte Then
        Me.Type = TypeSymbol.Byte
      ElseIf TypeOf value Is Boolean Then
        Me.Type = TypeSymbol.Boolean
      ElseIf TypeOf value Is Date Then
        Me.Type = TypeSymbol.DateTime
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
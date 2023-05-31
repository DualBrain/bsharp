Imports Bsharp.CodeAnalysis.Binding

Namespace Bsharp.CodeAnalysis.Symbols

  Public MustInherit Class VariableSymbol
    Inherits Symbol

    Friend Sub New(name As String, isArray As Boolean, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression)
      MyBase.New(name)
      Me.IsReadOnly = False
      Me.IsArray = isArray
      Me.Type = type
      Me.Constant = Nothing
      Me.Lower = lower
      Me.Upper = upper
    End Sub

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant)
      MyBase.New(name)
      Me.IsReadOnly = isReadOnly
      Me.Type = type
      Me.Constant = If(isReadOnly, constant, Nothing)
      Me.IsArray = False
      Me.Lower = Nothing
      Me.Upper = Nothing
    End Sub

    Public ReadOnly Property IsReadOnly As Boolean
    Public ReadOnly Property Type As TypeSymbol
    Friend ReadOnly Property Constant As BoundConstant
    Public ReadOnly Property IsArray As Boolean
    Friend ReadOnly Property Lower As BoundExpression
    Friend ReadOnly Property Upper As BoundExpression

  End Class

End Namespace
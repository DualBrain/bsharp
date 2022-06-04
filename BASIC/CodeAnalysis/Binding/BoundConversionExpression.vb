Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundConversionExpression
    Inherits BoundExpression

    Sub New([type] As TypeSymbol, expression As BoundExpression)
      Me.Type = [type]
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ConversionExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend Class BoundMidStatement
    Inherits BoundStatement

    Public Sub New(variable As VariableSymbol, positionExpression As BoundExpression, lengthExpression As BoundExpression, expression As BoundExpression)
      Me.Variable = variable
      Me.PositionExpression = positionExpression
      Me.LengthExpression = lengthExpression
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.MidStatement
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property PositionExpression As BoundExpression
    Public ReadOnly Property LengthExpression As BoundExpression
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
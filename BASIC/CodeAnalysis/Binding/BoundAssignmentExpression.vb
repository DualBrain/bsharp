Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundAssignmentExpression
    Inherits BoundExpression

    Public Sub New(variable As VariableSymbol, expression As BoundExpression)
      Me.Variable = variable
      Me.Expression = expression
      Me.Type = expression.Type
    End Sub

    Public Overrides ReadOnly Property Type As TypeSymbol
    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.AssignmentExpression
    Public ReadOnly Property Variable As VariableSymbol
    Public ReadOnly Property Expression As BoundExpression

  End Class

End Namespace
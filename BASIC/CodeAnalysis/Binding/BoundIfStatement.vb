Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundIfStatement
    Inherits BoundStatement

    Public Sub New(condition As BoundExpression, thenStatement As BoundStatement, elseStatement As BoundStatement)
      Me.Condition = condition
      Me.ThenStatement = thenStatement
      Me.Elsestatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.IfStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property ThenStatement As BoundStatement
    Public ReadOnly Property ElseStatement As BoundStatement

  End Class

End Namespace

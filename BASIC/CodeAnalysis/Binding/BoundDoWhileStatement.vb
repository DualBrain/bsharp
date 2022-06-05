Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDoWhileStatement
    Inherits BoundStatement

    Public Sub New(body As BoundStatement, condition As BoundExpression, atBeginning As Boolean)
      Me.Body = body
      Me.Condition = condition
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoWhileStatement
    Public ReadOnly Property Body As BoundStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property AtBeginning As Boolean

  End Class

  Friend NotInheritable Class BoundDoUntilStatement
    Inherits BoundStatement

    Public Sub New(body As BoundStatement, condition As BoundExpression, atBeginning As Boolean)
      Me.Body = body
      Me.Condition = condition
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoUntilStatement
    Public ReadOnly Property Body As BoundStatement
    Public ReadOnly Property Condition As BoundExpression
    Public ReadOnly Property AtBeginning As Boolean

  End Class

End Namespace
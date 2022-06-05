Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDoWhileStatement
    Inherits BoundLoopStatement

    Public Sub New(statements As BoundStatement, expression As BoundExpression, atBeginning As Boolean, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Statements = statements
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoWhileStatement
    Public ReadOnly Property Statements As BoundStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property AtBeginning As Boolean

  End Class

  Friend NotInheritable Class BoundDoUntilStatement
    Inherits BoundLoopStatement

    Public Sub New(statements As BoundStatement, expression As BoundExpression, atBeginning As Boolean, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Statements = statements
      Me.Expression = expression
      Me.AtBeginning = atBeginning
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DoUntilStatement
    Public ReadOnly Property Statements As BoundStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property AtBeginning As Boolean

  End Class

End Namespace
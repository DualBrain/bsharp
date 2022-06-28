Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundWhileStatement
    Inherits BoundLoopStatement

    Public Sub New(expression As BoundExpression, statements As BoundStatement, exitLabel As BoundLabel, continueLabel As BoundLabel)
      MyBase.New(exitLabel, continueLabel)
      Me.Expression = expression
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WhileStatement
    Public ReadOnly Property Expression As BoundExpression
    Public ReadOnly Property Statements As BoundStatement

  End Class

End Namespace

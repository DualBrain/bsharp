Namespace Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundLoopStatement
    Inherits BoundStatement

    Protected Sub New(exitLabel As BoundLabel, continueLabel As BoundLabel)
      Me.ExitLabel = exitLabel
      Me.ContinueLabel = continueLabel
    End Sub

    Public ReadOnly Property ExitLabel() As BoundLabel
    Public ReadOnly Property ContinueLabel() As BoundLabel

  End Class

End Namespace
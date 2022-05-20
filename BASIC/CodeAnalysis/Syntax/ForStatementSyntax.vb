Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ForStatementSyntax
    Inherits StatementSyntax

    Public Sub New(variable As SyntaxToken,
                   lowerBound As ExpressionSyntax,
                   upperBound As ExpressionSyntax,
                   stepper As ExpressionSyntax,
                   body As StatementSyntax)
      Me.Variable = variable
      Me.LowerBound = lowerBound
      Me.UpperBound = upperBound
      Me.Stepper = stepper
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ForStatement
    Public ReadOnly Property Variable As SyntaxToken
    Public ReadOnly Property LowerBound As ExpressionSyntax
    Public ReadOnly Property UpperBound As ExpressionSyntax
    Public ReadOnly Property Stepper As ExpressionSyntax
    Public ReadOnly Property Body As StatementSyntax

  End Class

End Namespace
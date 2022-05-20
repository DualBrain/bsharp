Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ExpressionStatementSyntax
    Inherits StatementSyntax

    ' valid
    ' ---------
    ' a = 10
    ' a += 1
    '
    ' not valid
    ' ---------
    ' a + 1

    Public Sub New(expression As ExpressionSyntax)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ExpressionStatement
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class DoWhileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(doKeyword As SyntaxToken, body As StatementSyntax, whileKeyword As SyntaxToken, condition As ExpressionSyntax)
      Me.DoKeyword = doKeyword
      Me.Body = body
      Me.WhileKeyword = whileKeyword
      Me.Condition = condition
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DoWhileStatement
    Public ReadOnly Property DoKeyword As SyntaxToken
    Public ReadOnly Property Body As StatementSyntax
    Public ReadOnly Property WhileKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax

  End Class

End Namespace
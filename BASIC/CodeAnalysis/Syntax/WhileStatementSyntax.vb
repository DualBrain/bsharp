Namespace Basic.CodeAnalysis.Syntax

  Friend Class WhileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(whileKeyword As SyntaxToken, condition As ExpressionSyntax, body As StatementSyntax)
      Me.WhileKeyword = whileKeyword
      Me.Condition = condition
      Me.Body = body
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WhileStatement
    Public ReadOnly Property WhileKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property Body As StatementSyntax

  End Class

End Namespace

Namespace Basic.CodeAnalysis.Syntax

  Friend Class WhileStatementSyntax
    Inherits StatementSyntax

    Public Sub New(whileKeyword As SyntaxToken, expression As ExpressionSyntax, body As StatementSyntax, wendKeyword As SyntaxToken)
      Me.WhileKeyword = whileKeyword
      Me.Expression = expression
      Me.Body = body
      Me.WendKeyword = wendKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.WhileStatement
    Public ReadOnly Property WhileKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property Body As StatementSyntax
    Public ReadOnly Property WendKeyword As SyntaxToken

  End Class

End Namespace

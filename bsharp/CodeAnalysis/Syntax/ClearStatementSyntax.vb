Namespace Bsharp.CodeAnalysis.Syntax

  Partial Friend Class ClearStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, clearKeyword As SyntaxToken, maxBytesCommaToken As SyntaxToken, maxBytesExpression As ExpressionSyntax, stackSpaceCommaToken As SyntaxToken, stackSpaceExpression As ExpressionSyntax)
      MyBase.New(tree)
      Me.ClearKeyword = clearKeyword
      Me.MaxBytesCommaToken = maxBytesCommaToken
      Me.MaxBytesExpression = maxBytesExpression
      Me.StackSpaceCommaToken = stackSpaceCommaToken
      Me.StackSpaceExpression = stackSpaceExpression
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ClearStatement
    Public ReadOnly Property ClearKeyword As SyntaxToken
    Public ReadOnly Property MaxBytesCommaToken As SyntaxToken
    Public ReadOnly Property MaxBytesExpression As ExpressionSyntax
    Public ReadOnly Property StackSpaceCommaToken As SyntaxToken
    Public ReadOnly Property StackSpaceExpression As ExpressionSyntax

  End Class

End Namespace
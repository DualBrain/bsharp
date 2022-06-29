Namespace Bsharp.CodeAnalysis.Syntax

  Partial Friend Class ClsStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, clsKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.ClsKeyword = clsKeyword
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ClsStatement
    Public ReadOnly Property ClsKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
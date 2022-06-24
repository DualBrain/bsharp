Namespace Basic.CodeAnalysis.Syntax

  Partial Friend Class TabFunctionSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, tabKeyword As SyntaxToken, openParen As SyntaxToken, expression As ExpressionSyntax, closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.TabKeyword = tabKeyword
      Me.OpenParen = openParen
      Me.Expression = expression
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.TabFunction
    Public ReadOnly Property TabKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

End Namespace
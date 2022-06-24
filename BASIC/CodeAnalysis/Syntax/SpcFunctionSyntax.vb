Namespace Basic.CodeAnalysis.Syntax

  Partial Friend Class SpcFunctionSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, spcKeyword As SyntaxToken, openParen As SyntaxToken, expression As ExpressionSyntax, closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.SpcKeyword = spcKeyword
      Me.OpenParen = openParen
      Me.Expression = expression
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.SpcFunction
    Public ReadOnly Property SpcKeyword As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

End Namespace
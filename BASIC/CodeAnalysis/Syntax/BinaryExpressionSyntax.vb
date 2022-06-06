Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class BinaryExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, left As ExpressionSyntax, operatorToken As SyntaxToken, right As ExpressionSyntax)
      MyBase.New(tree)
      Me.Left = left
      Me.OperatorToken = operatorToken
      Me.Right = right
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BinaryExpression
    Public ReadOnly Property Left As ExpressionSyntax
    Public ReadOnly Property OperatorToken As SyntaxToken
    Public ReadOnly Property Right As ExpressionSyntax

  End Class

End Namespace
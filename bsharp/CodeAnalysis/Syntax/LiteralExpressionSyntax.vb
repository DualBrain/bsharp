Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class LiteralExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, literalToken As SyntaxToken)
      Me.New(tree, literalToken, literalToken.Value)
    End Sub

    Sub New(tree As SyntaxTree, literalToken As SyntaxToken, value As Object)
      MyBase.New(tree)
      Me.LiteralToken = literalToken
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LiteralExpression
    Public ReadOnly Property LiteralToken As SyntaxToken
    Public ReadOnly Property Value As Object

  End Class

End Namespace
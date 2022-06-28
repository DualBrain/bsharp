Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class AssignmentExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, identifierToken As SyntaxToken, equalsToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.IdentifierToken = identifierToken
      Me.EqualToken = equalsToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.AssignmentExpression
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
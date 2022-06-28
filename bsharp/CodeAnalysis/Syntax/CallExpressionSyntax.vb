Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class CallExpressionSyntax
    Inherits ExpressionSyntax

    Sub New(tree As SyntaxTree, identifier As SyntaxToken, openParenToken As SyntaxToken, arguments As SeparatedSyntaxList(Of ExpressionSyntax), closeParenToken As SyntaxToken)
      MyBase.New(tree)
      Me.Identifier = identifier
      Me.OpenParenToken = openParenToken
      Me.Arguments = arguments
      Me.CloseParenToken = closeParenToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CallExpression
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Arguments As SeparatedSyntaxList(Of ExpressionSyntax)
    Public ReadOnly Property CloseParenToken As SyntaxToken

  End Class

End Namespace
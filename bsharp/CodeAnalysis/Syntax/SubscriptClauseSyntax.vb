Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class SubscriptClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, openParenToken As SyntaxToken, lower As LowerSubscriptClauseSyntax, upper As ExpressionSyntax, closeParen As SyntaxToken)
      MyBase.New(tree)
      Me.OpenParenToken = openParenToken
      Me.Lower = lower
      Me.Upper = upper
      Me.CloseParen = closeParen
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SubscriptClause
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Lower As LowerSubscriptClauseSyntax
    Public ReadOnly Property Upper As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken

  End Class

  Partial Public NotInheritable Class LowerSubscriptClauseSyntax
    Inherits SyntaxNode

    Sub New(tree As SyntaxTree, lower As ExpressionSyntax, toKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.Lower = lower
      Me.ToKeyword = toKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LowerSubscriptClause
    Public ReadOnly Property Lower As ExpressionSyntax
    Public ReadOnly Property ToKeyword As SyntaxToken

  End Class

End Namespace
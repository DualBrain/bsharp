Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class NameExpressionSyntax
    Inherits ExpressionSyntax

    Public Sub New(tree As SyntaxTree, identifierToken As SyntaxToken)
      MyBase.New(tree)
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NameExpression
    Public ReadOnly Property IdentifierToken As SyntaxToken

  End Class

End Namespace
Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class ReturnStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, returnKeyword As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.ReturnKeyword = returnKeyword
      Me.Expression = expression
      Me.IdentifierToken = Nothing
    End Sub

    Public Sub New(tree As SyntaxTree, returnKeyword As SyntaxToken, identifierToken As SyntaxToken)
      MyBase.New(tree)
      Me.ReturnKeyword = returnKeyword
      Me.Expression = Nothing
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ReturnStatement
    Public ReadOnly Property ReturnKeyword() As SyntaxToken
    Public ReadOnly Property Expression() As ExpressionSyntax
    Public ReadOnly Property IdentifierToken() As SyntaxToken

  End Class

End Namespace
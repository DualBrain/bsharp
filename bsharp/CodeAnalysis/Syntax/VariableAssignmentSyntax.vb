﻿Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class LetStatementSyntax
    Inherits StatementSyntax

    ' LET x = 10
    ' LET a$ = "test"
    ' LET v = 1.111

    Public Sub New(tree As SyntaxTree, letToken As SyntaxToken, identifierToken As SyntaxToken, openParenToken As SyntaxToken, arguments As SeparatedSyntaxList(Of ExpressionSyntax), closeParenToken As SyntaxToken, equalToken As SyntaxToken, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.LetToken = letToken
      Me.IdentifierToken = identifierToken
      Me.OpenParenToken = openParenToken
      Me.Arguments = arguments
      Me.CloseParenToken = closeParenToken
      Me.EqualToken = equalToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.LetStatement
    Public ReadOnly Property LetToken As SyntaxToken
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Arguments As SeparatedSyntaxList(Of ExpressionSyntax)
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
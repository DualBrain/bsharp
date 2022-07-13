Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class InputStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, inputKeyword As SyntaxToken, optionalSemiColonToken As SyntaxToken, optionalPromptExpression As ExpressionSyntax, semiColonOrCommaToken As SyntaxToken, tokens As ImmutableArray(Of SyntaxToken))
      MyBase.New(tree)
      Me.InputKeyword = inputKeyword
      Me.OptionalSemiColonToken = optionalSemiColonToken
      Me.OptionalPromptExpression = optionalPromptExpression
      Me.SemiColonOrCommaToken = semiColonOrCommaToken
      Me.Tokens = tokens
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.InputStatement
    Public ReadOnly Property InputKeyword As SyntaxToken
    Public ReadOnly Property OptionalSemiColonToken As SyntaxToken
    Public ReadOnly Property OptionalPromptExpression As ExpressionSyntax
    Public ReadOnly Property SemiColonOrCommaToken As SyntaxToken
    Public ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

  End Class

End Namespace
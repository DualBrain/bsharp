Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class GotoStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, gotoKeyword As SyntaxToken, identifierToken As SyntaxToken)
      MyBase.New(tree)
      Me.GotoKeyword = gotoKeyword
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.GotoStatement
    Public ReadOnly Property GotoKeyword() As SyntaxToken
    Public ReadOnly Property IdentifierToken() As SyntaxToken

  End Class

End Namespace
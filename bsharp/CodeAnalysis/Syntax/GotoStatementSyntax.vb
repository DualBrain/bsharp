Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class GotoStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, gotoKeyword As SyntaxToken, targetToken As SyntaxToken)
      MyBase.New(tree)
      Me.GotoKeyword = gotoKeyword
      Me.TargetToken = targetToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.GotoStatement
    Public ReadOnly Property GotoKeyword() As SyntaxToken
    Public ReadOnly Property TargetToken() As SyntaxToken

  End Class

  Partial Public NotInheritable Class GosubStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, gosubKeyword As SyntaxToken, identifierToken As SyntaxToken)
      MyBase.New(tree)
      Me.GosubKeyword = gosubKeyword
      Me.IdentifierToken = identifierToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.GosubStatement
    Public ReadOnly Property GosubKeyword() As SyntaxToken
    Public ReadOnly Property IdentifierToken() As SyntaxToken

  End Class

End Namespace
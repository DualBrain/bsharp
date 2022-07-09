Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class KillStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, killKeyword As SyntaxToken, path As ExpressionSyntax)
      MyBase.New(tree)
      Me.KillKeyword = killKeyword
      Me.Path = path
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.KillStatement
    Public ReadOnly Property KillKeyword As SyntaxToken
    Public ReadOnly Property Path As ExpressionSyntax

  End Class

End Namespace
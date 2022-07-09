Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class ChDirStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, chDirKeyword As SyntaxToken, path As ExpressionSyntax)
      MyBase.New(tree)
      Me.ChDirKeyword = chDirKeyword
      Me.Path = path
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ChDirStatement
    Public ReadOnly Property ChDirKeyword As SyntaxToken
    Public ReadOnly Property Path As ExpressionSyntax

  End Class

End Namespace
Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class MkDirStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, mkDirKeyword As SyntaxToken, path As ExpressionSyntax)
      MyBase.New(tree)
      Me.MkDirKeyword = mkDirKeyword
      Me.Path = path
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.MkDirStatement
    Public ReadOnly Property MkDirKeyword As SyntaxToken
    Public ReadOnly Property Path As ExpressionSyntax

  End Class

End Namespace
Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class RmDirStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, rmDirKeyword As SyntaxToken, path As ExpressionSyntax)
      MyBase.New(tree)
      Me.RmDirKeyword = rmDirKeyword
      Me.Path = path
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RmDirStatement
    Public ReadOnly Property RmDirKeyword As SyntaxToken
    Public ReadOnly Property Path As ExpressionSyntax

  End Class

End Namespace
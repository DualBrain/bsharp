Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class RemStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, remKeyword As SyntaxToken, comment As String)
      MyBase.New(tree)
      Me.RemKeyword = remKeyword
      Me.Comment = comment
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RemStatement
    Public ReadOnly Property RemKeyword As SyntaxToken
    Public ReadOnly Property Comment As String

  End Class

End Namespace
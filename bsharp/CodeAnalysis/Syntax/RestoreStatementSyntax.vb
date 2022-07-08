Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class RestoreStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, restoreKeyword As SyntaxToken, numberToken As SyntaxToken)
      MyBase.New(tree)
      Me.RestoreKeyword = restoreKeyword
      Me.NumberToken = numberToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.RestoreStatement
    Public ReadOnly Property RestoreKeyword As SyntaxToken
    Public ReadOnly Property NumberToken As SyntaxToken

  End Class

End Namespace

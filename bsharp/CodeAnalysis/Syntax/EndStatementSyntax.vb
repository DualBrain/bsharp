Namespace Bsharp.CodeAnalysis.Syntax

  Partial Friend Class EndStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, endKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.EndKeyword = endKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.EndStatement
    Public ReadOnly Property EndKeyword As SyntaxToken

  End Class

End Namespace
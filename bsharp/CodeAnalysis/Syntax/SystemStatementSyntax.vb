Namespace Bsharp.CodeAnalysis.Syntax

  Partial Friend Class SystemStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, systemKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.SystemKeyword = systemKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.SystemStatement
    Public ReadOnly Property SystemKeyword As SyntaxToken

  End Class

End Namespace
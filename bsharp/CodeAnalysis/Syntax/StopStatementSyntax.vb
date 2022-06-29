Namespace Bsharp.CodeAnalysis.Syntax

  Partial Friend Class StopStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, stopKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.StopKeyword = stopKeyword
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.StopStatement
    Public ReadOnly Property StopKeyword As SyntaxToken

  End Class

End Namespace
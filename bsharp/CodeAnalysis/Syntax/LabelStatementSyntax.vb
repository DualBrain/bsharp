Namespace Bsharp.CodeAnalysis.Syntax

  Partial Friend Class LabelStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, label As SyntaxToken)
      MyBase.New(tree)
      Me.Label = label
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.LabelStatement
    Public ReadOnly Property Label As SyntaxToken

  End Class

End Namespace
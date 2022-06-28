Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class ExpressionStatementSyntax
    Inherits StatementSyntax

    ' valid
    ' ---------
    ' a = 10
    ' a += 1
    '
    ' not valid
    ' ---------
    ' a + 1

    Public Sub New(tree As SyntaxTree, expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ExpressionStatement
    Public ReadOnly Property Expression As ExpressionSyntax

  End Class

End Namespace
Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class BlockStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree, openBraceToken As SyntaxToken, statements As ImmutableArray(Of StatementSyntax), closeBraceToken As SyntaxToken)
      MyBase.New(tree)
      Me.OpenBraceToken = openBraceToken
      Me.Statements = statements
      Me.CloseBraceToken = closeBraceToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BlockStatement
    Public ReadOnly Property OpenBraceToken As SyntaxToken
    Public ReadOnly Property Statements As ImmutableArray(Of StatementSyntax)
    Public ReadOnly Property CloseBraceToken As SyntaxToken

  End Class

End Namespace
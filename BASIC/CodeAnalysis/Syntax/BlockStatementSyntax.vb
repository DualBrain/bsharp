Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class BlockStatementSyntax
    Inherits StatementSyntax

    Public Sub New(openBraceToken As SyntaxToken, statements As ImmutableArray(Of StatementSyntax), closedBraceToken As SyntaxToken)
      Me.OpenBraceToken = openBraceToken
      Me.Statements = statements
      Me.ClosedBraceToken = closedBraceToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BlockStatement
    Public ReadOnly Property OpenBraceToken As SyntaxToken
    Public ReadOnly Property Statements As ImmutableArray(Of StatementSyntax)
    Public ReadOnly Property ClosedBraceToken As SyntaxToken

  End Class

End Namespace
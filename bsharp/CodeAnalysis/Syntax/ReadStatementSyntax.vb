Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class ReadStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, readKeyword As SyntaxToken, tokens As ImmutableArray(Of SyntaxToken))
      MyBase.New(tree)
      Me.ReadKeyword = readKeyword
      Me.Tokens = tokens
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ReadStatement
    Public ReadOnly Property ReadKeyword As SyntaxToken
    Public ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

  End Class

End Namespace
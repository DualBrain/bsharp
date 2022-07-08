Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class DataStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, dataKeyword As SyntaxToken, tokens As ImmutableArray(Of SyntaxToken))
      MyBase.New(tree)
      Me.DataKeyword = dataKeyword
      Me.Tokens = tokens
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.DataStatement
    Public ReadOnly Property DataKeyword As SyntaxToken
    Public ReadOnly Property Tokens As ImmutableArray(Of SyntaxToken)

  End Class

End Namespace

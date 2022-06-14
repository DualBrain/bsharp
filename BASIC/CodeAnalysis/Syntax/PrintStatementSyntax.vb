Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class PrintStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, printKeyword As SyntaxToken, nodes As ImmutableArray(Of SyntaxNode))
      MyBase.New(tree)
      Me.PrintKeyword = printKeyword
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.PrintStatement
    Public ReadOnly Property PrintKeyword() As SyntaxToken
    Public ReadOnly Property Nodes() As ImmutableArray(Of SyntaxNode)

  End Class

End Namespace
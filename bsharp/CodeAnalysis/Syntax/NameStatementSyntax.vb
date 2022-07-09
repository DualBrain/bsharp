Namespace Bsharp.CodeAnalysis.Syntax

  Friend Class NameStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, nameKeyword As SyntaxToken, originalPath As ExpressionSyntax, asKeyword As SyntaxToken, destinationPath As ExpressionSyntax)
      MyBase.New(tree)
      Me.NameKeyword = nameKeyword
      Me.OriginalPath = originalPath
      Me.AsKeyword = asKeyword
      Me.DestinationPath = destinationPath
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.NameStatement
    Public ReadOnly Property NameKeyword As SyntaxToken
    Public ReadOnly Property OriginalPath As ExpressionSyntax
    Public ReadOnly Property AsKeyword As SyntaxToken
    Public ReadOnly Property DestinationPath As ExpressionSyntax

  End Class

End Namespace
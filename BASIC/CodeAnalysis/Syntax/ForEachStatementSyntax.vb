Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ForEachStatementSyntax
    Inherits StatementSyntax

    Public Sub New(forKeyword As SyntaxToken,
                   eachKeyword As SyntaxToken,
                   value As SyntaxToken,
                   inKeyword As SyntaxToken,
                   array As SyntaxToken,
                   statements As BlockStatementSyntax,
                   nextKeyword As SyntaxToken)
      Me.ForKeyword = forKeyword
      Me.EachKeyword = eachKeyword
      Me.Value = value
      Me.InKeyword = inKeyword
      Me.Array = array
      Me.Statements = statements
      Me.NextKeyword = nextKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ForEachStatement
    Public ReadOnly Property ForKeyword As SyntaxToken
    Public ReadOnly Property EachKeyword As SyntaxToken
    Public ReadOnly Property Value As SyntaxToken
    Public ReadOnly Property InKeyword As SyntaxToken
    Public ReadOnly Property Array As SyntaxToken
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property NextKeyword As SyntaxToken

  End Class

End Namespace
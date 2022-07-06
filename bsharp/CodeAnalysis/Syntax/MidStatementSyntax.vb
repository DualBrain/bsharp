Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class MidStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   midKeyword As SyntaxToken,
                   openParen As SyntaxToken,
                   identifierToken As SyntaxToken,
                   positionCommaToken As SyntaxToken,
                   position As ExpressionSyntax,
                   lengthCommaToken As SyntaxToken,
                   length As ExpressionSyntax,
                   closeParen As SyntaxToken,
                   equalToken As SyntaxToken,
                   expression As ExpressionSyntax)
      MyBase.New(tree)
      Me.MidKeyword = midKeyword
      Me.OpenParen = openParen
      Me.IdentifierToken = identifierToken
      Me.PositionCommaToken = positionCommaToken
      Me.PositionExpression = position
      Me.LengthCommaToken = lengthCommaToken
      Me.LengthExpression = length
      Me.CloseParen = closeParen
      Me.EqualToken = equalToken
      Me.Expression = expression
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.MidStatement
    Public ReadOnly Property MidKeyword() As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property PositionCommaToken As SyntaxToken
    Public ReadOnly Property PositionExpression As ExpressionSyntax
    Public ReadOnly Property LengthCommaToken As SyntaxToken
    Public ReadOnly Property LengthExpression As ExpressionSyntax
    Public ReadOnly Property CloseParen As SyntaxToken
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
  End Class

End Namespace
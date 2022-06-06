Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SingleLineIfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, ifKeyword As SyntaxToken,
                   expression As ExpressionSyntax,
                   thenKeyword As SyntaxToken,
                   statements As StatementSyntax,
                   elseClause As SingleLineElseClauseSyntax)
      MyBase.New(tree)
      Me.IfKeyword = ifKeyword
      Me.Expression = expression
      Me.ThenKeyword = thenKeyword
      Me.Statements = statements
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SingleLineIfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property ThenKeyword As SyntaxToken
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property ElseClause As SingleLineElseClauseSyntax

  End Class

  Public NotInheritable Class SingleLineElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree, elseKeyword As SyntaxToken, statements As StatementSyntax)
      MyBase.New(tree)
      Me.ElseKeyword = elseKeyword
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SingleLineElseClause
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property Statements As StatementSyntax

  End Class

  'Public NotInheritable Class MultiLineIfBlock
  '  Inherits StatementSyntax

  '  'elseIfStatements As ImmutableArray(Of ElseIfStatementSyntax),
  '  Public Sub New(tree As SyntaxTree,
  '                 ifStatement As IfStatementSyntax,
  '                 elseStatement As ElseStatementSyntax,
  '                 endKeyword As SyntaxToken,
  '                 closingKeyword As SyntaxToken)
  '    MyBase.New(tree)
  '    Me.IfStatement = ifStatement
  '    'Me.ElseIfStatements = ElseIfStatements
  '    Me.ElseStatement = elseStatement
  '    Me.EndKeyword = endKeyword
  '    Me.ClosingKeyword = closingKeyword
  '  End Sub

  '  Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.MultiLineIfBlock
  '  Public ReadOnly Property IfStatement As IfStatementSyntax
  '  'Public ReadOnly Property ElseIfStatements As ImmutableArray(Of ElseIfStatementSyntax)
  '  Public ReadOnly Property ElseStatement As ElseStatementSyntax
  '  Public ReadOnly Property EndKeyword As SyntaxToken
  '  Public ReadOnly Property ClosingKeyword As SyntaxToken

  'End Class

  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   ifKeyword As SyntaxToken,
                   expression As ExpressionSyntax,
                   thenKeyword As SyntaxToken,
                   statements As StatementSyntax,
                   elseClause As ElseClauseSyntax,
                   endIfKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.IfKeyword = ifKeyword
      Me.Expression = expression
      Me.ThenKeyword = thenKeyword
      Me.Statements = statements
      Me.ElseClause = elseClause
      Me.EndIfKeyword = endIfKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Expression As ExpressionSyntax
    Public ReadOnly Property ThenKeyword As SyntaxToken
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property ElseClause As ElseClauseSyntax
    Public ReadOnly Property EndIfKeyword As SyntaxToken

  End Class

  'Public NotInheritable Class ElseIfStatementSyntax
  '  Inherits SyntaxNode

  '  Public Sub New(tree As SyntaxTree, elseIfKeyword As SyntaxToken, expression As ExpressionSyntax, thenKeyword As SyntaxToken, statements As StatementSyntax)
  '    MyBase.New(tree)
  '    Me.ElseIfKeyword = elseIfKeyword
  '    Me.Expression = expression
  '    Me.ThenKeyword = thenKeyword
  '    Me.Statements = statements
  '  End Sub

  '  Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseIfStatement
  '  Public ReadOnly Property ElseIfKeyword As SyntaxToken
  '  Public ReadOnly Property Expression As ExpressionSyntax
  '  Public ReadOnly Property ThenKeyword As SyntaxToken
  '  Public ReadOnly Property Statements As StatementSyntax

  'End Class

  Public NotInheritable Class ElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree, elseKeyword As SyntaxToken, statements As StatementSyntax)
      MyBase.New(tree)
      Me.ElseKeyword = elseKeyword
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseStatement
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property Statements As StatementSyntax

  End Class

End Namespace
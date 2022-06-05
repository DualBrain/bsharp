﻿Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SingleLineIfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(ifKeyword As SyntaxToken,
                   condition As ExpressionSyntax,
                   thenKeyword As SyntaxToken,
                   thenStatement As StatementSyntax,
                   elseClause As SingleLineElseClauseSyntax)
      Me.IfKeyword = ifKeyword
      Me.Condition = condition
      Me.ThenKeyword = thenKeyword
      Me.ThenStatement = thenStatement
      Me.ElseClause = elseClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SingleLineIfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property ThenKeyword As SyntaxToken
    Public ReadOnly Property ThenStatement As StatementSyntax
    Public ReadOnly Property ElseClause As SingleLineElseClauseSyntax

  End Class

  Public NotInheritable Class SingleLineElseClauseSyntax
    Inherits SyntaxNode

    Public Sub New(elseKeyword As SyntaxToken, elseStatement As StatementSyntax)
      Me.ElseKeyword = elseKeyword
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SingleLineElseClause
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property ElseStatement As StatementSyntax

  End Class

  Public NotInheritable Class MultiLineIfBlock
    Inherits StatementSyntax

    Public Sub New(ifStatement As IfStatementSyntax,
                   elseIfStatements As ImmutableArray(Of ElseIfStatementSyntax),
                   elseStatement As ElseStatementSyntax,
                   endKeyword As SyntaxToken,
                   closingKeyword As SyntaxToken)
      Me.IfStatement = ifStatement
      Me.ElseIfStatements = elseIfStatements
      Me.ElseStatement = elseStatement
      Me.EndKeyword = endKeyword
      Me.ClosingKeyword = closingKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.MultiLineIfBlock
    Public ReadOnly Property IfStatement As IfStatementSyntax
    Public ReadOnly Property ElseIfStatements As ImmutableArray(Of ElseIfStatementSyntax)
    Public ReadOnly Property ElseStatement As ElseStatementSyntax
    Public ReadOnly Property EndKeyword As SyntaxToken
    Public ReadOnly Property ClosingKeyword As SyntaxToken

  End Class

  Public NotInheritable Class IfStatementSyntax
    Inherits StatementSyntax

    Public Sub New(ifKeyword As SyntaxToken,
                   condition As ExpressionSyntax,
                   thenKeyword As SyntaxToken,
                   thenStatement As StatementSyntax)
      Me.IfKeyword = ifKeyword
      Me.Condition = condition
      Me.ThenKeyword = thenKeyword
      Me.ThenStatement = thenStatement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.IfStatement
    Public ReadOnly Property IfKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property ThenKeyword As SyntaxToken
    Public ReadOnly Property ThenStatement As StatementSyntax

  End Class

  Public NotInheritable Class ElseIfStatementSyntax
    Inherits SyntaxNode

    Public Sub New(elseIfKeyword As SyntaxToken, condition As ExpressionSyntax, thenKeyword As SyntaxToken, statement As StatementSyntax)
      Me.ElseIfKeyword = elseIfKeyword
      Me.Condition = condition
      Me.ThenKeyword = thenKeyword
      Me.Statement = statement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseIfStatement
    Public ReadOnly Property ElseIfKeyword As SyntaxToken
    Public ReadOnly Property Condition As ExpressionSyntax
    Public ReadOnly Property ThenKeyword As SyntaxToken
    Public ReadOnly Property Statement As StatementSyntax

  End Class

  Public NotInheritable Class ElseStatementSyntax
    Inherits SyntaxNode

    Public Sub New(elseKeyword As SyntaxToken, elseStatement As StatementSyntax)
      Me.ElseKeyword = elseKeyword
      Me.ElseStatement = elseStatement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ElseStatement
    Public ReadOnly Property ElseKeyword As SyntaxToken
    Public ReadOnly Property ElseStatement As StatementSyntax

  End Class

End Namespace
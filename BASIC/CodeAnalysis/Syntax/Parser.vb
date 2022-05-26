Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Parser

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_text As SourceText
    Private ReadOnly m_tokens As ImmutableArray(Of SyntaxToken)

    Private m_position As Integer

    Public Sub New(text As SourceText)

      Dim tokens = New List(Of SyntaxToken)
      Dim lexer = New Lexer(text)
      Dim token As SyntaxToken
      Do

        token = lexer.Lex

        If token.Kind <> SyntaxKind.WhitespaceToken AndAlso
         token.Kind <> SyntaxKind.BadToken Then
          tokens.Add(token)
        End If

      Loop While token.Kind <> SyntaxKind.EndOfFileToken

      m_text = text
      m_tokens = tokens.ToImmutableArray
      m_diagnostics.AddRange(lexer.Diagnostics)

    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag
      Get
        Return m_diagnostics
      End Get
    End Property

    Private Function Peek(offset As Integer) As SyntaxToken
      Dim index = m_position + offset
      If index >= m_tokens.Length Then
        Return m_tokens(m_tokens.Length - 1)
      End If
      Return m_tokens(index)
    End Function

    Private Function Current() As SyntaxToken
      Return Peek(0)
    End Function

    Private Function NextToken() As SyntaxToken
      Dim current = Me.Current
      m_position += 1
      Return current
    End Function

    Private Function MatchToken(kind As SyntaxKind) As SyntaxToken
      If Current.Kind = kind Then
        Return NextToken()
      End If
      m_diagnostics.ReportUnexpectedToken(Current.Span, Current.Kind, kind)
      Return New SyntaxToken(kind, Current.Position, Nothing, Nothing)
    End Function

    Public Function ParseCompilationUnit() As CompilationUnitSyntax
      Dim statement = ParseStatement()
      Dim endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken)
      Return New CompilationUnitSyntax(statement, endOfFileToken)
    End Function

    Private Function ParseStatement() As StatementSyntax
      Select Case Current.Kind
        Case SyntaxKind.OpenBraceToken : Return ParseBlockStatement()
          '??? maybe have to tread differently? Case SyntaxKind.LetKeyword : Return ParseAssignmentExpression()
        'Case SyntaxKind.LetKeyword, SyntaxKind.DimKeyword, SyntaxKind.ConstKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.DimKeyword, SyntaxKind.ConstKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.IfKeyword : Return ParseIfStatement()
        Case SyntaxKind.WhileKeyword : Return ParseWhileStatement()
        Case SyntaxKind.ForKeyword : Return ParseForStatement()
        Case Else : Return ParseExpressionStatement()
      End Select
    End Function

    Private Function ParseVariableDeclaration() As StatementSyntax
      Dim expected = SyntaxKind.DimKeyword ' SyntaxKind.LetKeyword ' Default to let; eventually will infer.
      If Current.Kind = SyntaxKind.ConstKeyword Then expected = SyntaxKind.ConstKeyword
      If Current.Kind = SyntaxKind.DimKeyword Then expected = SyntaxKind.DimKeyword
      'If Current.Kind = SyntaxKind.LetKeyword Then expected = SyntaxKind.LetKeyword
      Dim keyword = MatchToken(expected)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim equals = MatchToken(SyntaxKind.EqualToken)
      Dim initializer = ParseExpression()
      Return New VariableDeclarationSyntax(keyword, identifier, equals, initializer)
    End Function

    Private Function ParseIfStatement() As StatementSyntax

      ' NOTES:
      '  - `End If` is not required for single line If/Then or If/Then/Else
      '  - But `End If` is required in all other scenarios.
      '  - Else is optional.
      '  - ElseIf is optional, but can exist 0 or more instances.
      '  - All {statement} blocks can be empty.
      '  - {condition} is always required and is evaluated as a Boolean.

      '  - Apparently `IF {condition} Then {statement} ELSE     <- with nothing else
      '    is completely valid in QB and QB64....

      '   If {condition} Then {statement}
      '   If {condition} Then {statement} Else {statement}
      '   If {condition} Then End If
      '   If {condition} Then {statement} End If
      '   If {condition} Then {statement} Else {statement} End If
      ' ? If {condition} Then {statement} ElseIf {condition} End If
      ' ? If {condition} Then {statement} ElseIf {condition} Then {statement} ElseIf {condition} Then {statement} End If
      ' ? If {condition} Then {statement} ElseIf {condition} Then {statement} Else {statement} End If
      ' ? If {condition} Then {statement} ElseIf {condition} Then {statement} ElseIf {condition} Then {statement} Else {statement} End If

      ' When parsing a IF statement,
      ' need to determine which type style...
      ' single-line or multi-line.
      ' Single-line can have an optional (empty) ELSE clause.

      ' In all cases, IF, condition, and THEN are required...
      Dim beginningIfKeyword = MatchToken(SyntaxKind.IfKeyword)
      Dim condition = ParseExpression()
      Dim thenKeyword = MatchToken(SyntaxKind.ThenKeyword)

      ' At this point we need to determine if
      ' there is either a CR/LF or comment.

      Dim thenLine = m_text.GetLineIndex(thenKeyword.Span.Start)
      Dim peekLine = m_text.GetLineIndex(Peek(0).Span.Start)

      Dim multiLine = peekLine > thenLine

      Dim statements = ParseStatements()

      Dim elseExists = (Peek(0).Kind = SyntaxKind.ElseKeyword)

      'TODO: Need to handle 0 or more ElseIf blocks.

      Dim elseClause = ParseElseClause()

      If multiLine Then
        Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
        Dim endingIfkeyword = MatchToken(SyntaxKind.IfKeyword)
      End If

      'If Not endIfExists AndAlso
      '   (statement Is Nothing OrElse (elseExists AndAlso elseClause Is Nothing)) Then

      '  ' Appears to be a one-liner IF statement but is missing either (or both) of the bodies.

      '  If statement Is Nothing OrElse
      '     (elseExists AndAlso elseClause Is Nothing) Then
      '    m_diagnostics.ReportMissingEndIf(Current.Span)
      '  End If

      'End If

      Return New IfStatementSyntax(beginningIfKeyword, condition, statements, elseClause)

    End Function

    Private Function ParseElseClause() As ElseClauseSyntax
      If Current.Kind <> SyntaxKind.ElseKeyword Then
        Return Nothing
      End If
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseStatements()
      Return New ElseClauseSyntax(elseKeyword, statements)
    End Function

    Private Function ParseWhileStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim condition = ParseExpression()
      Dim statement = ParseStatement()
      If Peek(0).Kind = SyntaxKind.WendKeyword Then
        Dim wendKeyword = MatchToken(SyntaxKind.WendKeyword)
      Else
        Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
        Dim endingWhilekeyword = MatchToken(SyntaxKind.WhileKeyword)
      End If
      Return New WhileStatementSyntax(keyword, condition, statement)
    End Function

    Private Function ParseForStatement() As StatementSyntax
      'TODO: Probably start here to parse `For Each ...`
      Dim keyword = MatchToken(SyntaxKind.ForKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim equalsToken = MatchToken(SyntaxKind.EqualToken)
      Dim lowerBound = ParseExpression()
      Dim toToken = MatchToken(SyntaxKind.ToKeyword)
      Dim upperBound = ParseExpression()
      Dim stepper As ExpressionSyntax = Nothing
      If Peek(0).Kind = SyntaxKind.StepKeyword Then
        Dim stepToken = MatchToken(SyntaxKind.StepKeyword)
        stepper = ParseExpression()
      End If
      Dim body = ParseStatement()
      Dim nextToken = MatchToken(SyntaxKind.NextKeyword)
      Return New ForStatementSyntax(identifier, lowerBound, upperBound, stepper, body)
    End Function

    Private Function ParseExpressionStatement() As ExpressionStatementSyntax
      Dim expression = ParseExpression()
      Return New ExpressionStatementSyntax(expression)
    End Function

    Private Function ParseBlockStatement() As BlockStatementSyntax
      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
      Dim openBraceToken = MatchToken(SyntaxKind.OpenBraceToken)
      Dim startToken = Current()
      While Current.Kind <> SyntaxKind.EndOfFileToken AndAlso
            Current.Kind <> SyntaxKind.CloseBraceToken

        startToken = Current()
        Dim statement = ParseStatement()
        statements.Add(statement)

        ' If ParseStatement did not consume any tokens,
        ' let's skip the current token and continue in 
        ' order to avoid an infinite loop.
        '
        ' We do not need to report an error, because we've
        ' already tried to parse an experession statement
        ' and reported one.
        If Current() Is startToken Then
          NextToken()
        End If

      End While
      Dim closeBraceToken = MatchToken(SyntaxKind.CloseBraceToken)
      Return New BlockStatementSyntax(openBraceToken, statements.ToImmutable, closeBraceToken)
    End Function

    Private Function ParseStatements() As ImmutableArray(Of StatementSyntax)
      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
      Dim startToken = Current()
      While True 'Current.Kind <> SyntaxKind.EndOfFileToken AndAlso
        'Current.Kind <> SyntaxKind.CloseBraceToken

        Select Case Current.Kind
          Case SyntaxKind.CloseBraceToken,
               SyntaxKind.ElseKeyword,
               SyntaxKind.ElseIfKeyword,
               SyntaxKind.EndKeyword,
               SyntaxKind.NextKeyword,
               SyntaxKind.EndOfFileToken
            Exit While
          Case Else
        End Select

        startToken = Current()
        Dim statement = ParseStatement()
        statements.Add(statement)

        ' If ParseStatement did not consume any tokens,
        ' let's skip the current token and continue in 
        ' order to avoid an infinite loop.
        '
        ' We do not need to report an error, because we've
        ' already tried to parse an experession statement
        ' and reported one.
        If Current() Is startToken Then
          NextToken()
        End If

      End While
      Return statements.ToImmutable
    End Function

    Private Function ParseExpression() As ExpressionSyntax
      Return ParseAssignmentExpression()
    End Function

    Private Function ParseAssignmentExpression() As ExpressionSyntax

      If SyntaxFacts.GetKeywordKind(Current.Text) = SyntaxKind.LetKeyword Then
        Dim letKeyword = NextToken()
        If Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
           Peek(1).Kind = SyntaxKind.EqualToken Then
          Dim identifierToken = NextToken()
          Dim operatorToken = NextToken()
          Dim right = ParseAssignmentExpression()
          Return New AssignmentExpressionSyntax(identifierToken, operatorToken, right)
        Else
          Stop
        End If
      End If

      Return ParseBinaryExpression()

    End Function

    Private Function ParseBinaryExpression(Optional parentPrecedence As Integer = 0) As ExpressionSyntax

      Dim left As ExpressionSyntax
      Dim unaryOperatorPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(Current.Kind)
      If unaryOperatorPrecedence <> 0 AndAlso unaryOperatorPrecedence > parentPrecedence Then
        Dim operatorToken = NextToken()
        Dim operand = ParseBinaryExpression(unaryOperatorPrecedence)
        left = New UnaryExpressionSyntax(operatorToken, operand)
      Else
        left = ParsePrimaryExpression()
      End If
      Do
        Dim precedence = SyntaxFacts.GetBinaryOperatorPrecedence(Current.Kind)
        If precedence = 0 OrElse precedence <= parentPrecedence Then Exit Do
        Dim operatorToken = NextToken()
        Dim right = ParseBinaryExpression(precedence)
        left = New BinaryExpressionSyntax(left, operatorToken, right)
      Loop
      Return left
    End Function

    Private Function ParsePrimaryExpression() As ExpressionSyntax
      Select Case Current.Kind
        Case SyntaxKind.OpenParenToken : Return ParseParenExpression()
        Case SyntaxKind.FalseKeyword, SyntaxKind.TrueKeyword : Return ParseBooleanLiteral()
        Case SyntaxKind.NumberToken : Return ParseNumberLiteral()
        Case Else : Return ParseNameExpression() 'Case SyntaxKind.IdentifierToken
      End Select
    End Function

    Private Function ParseParenExpression() As ExpressionSyntax
      Dim left = MatchToken(SyntaxKind.OpenParenToken)
      Dim expression = ParseExpression()
      Dim right = MatchToken(SyntaxKind.CloseParenToken)
      Return New ParenthesizedExpressionSyntax(left, expression, right)
    End Function

    Private Function ParseBooleanLiteral() As ExpressionSyntax
      Dim isTrue = (Current.Kind = SyntaxKind.TrueKeyword)
      Dim keywordToken = MatchToken(If(isTrue, SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword))
      Return New LiteralExpressionSyntax(keywordToken, isTrue)
    End Function

    Private Function ParseNumberLiteral() As ExpressionSyntax
      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(numberToken)
    End Function

    Private Function ParseNameExpression() As ExpressionSyntax
      Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
      Return New NameExpressionSyntax(identifierToken)
    End Function

  End Class

End Namespace
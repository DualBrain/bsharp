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
      'Dim statement = ParseStatement()
      Dim members = ParseMembers()
      Dim endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken)
      'Return New CompilationUnitSyntax(statement, endOfFileToken)
      Return New CompilationUnitSyntax(members, endOfFileToken)
    End Function

    Private Function ParseMembers() As ImmutableArray(Of MemberSyntax)

      Dim members = ImmutableArray.CreateBuilder(Of MemberSyntax)()

      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim startToken = Current()

        Dim member = ParseMember()
        members.Add(member)

        ' If ParseMember() did not consume any tokens,
        ' we need to skip the current token and continue
        ' in order to avoid an infinite loop.
        '
        ' We don't need to report an error, because we'll
        ' already tried to parse an expression statement
        ' and reported one.
        If Current() Is startToken Then
          NextToken()
        End If

      End While

      Return members.ToImmutable()

    End Function

    Private Function ParseMember() As MemberSyntax
      If Current.Kind = SyntaxKind.FunctionKeyword Then Return ParseFunctionDeclaration()
      Return ParseGlobalStatement()
    End Function

    Private Function ParseFunctionDeclaration() As MemberSyntax
      ' function <identifier>(<param>)
      '   <body>
      ' end function
      Dim functionKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim parameters = ParseParameterList()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Dim type = ParseOptionalTypeClause()
      Dim body = ParseBlockStatement()
      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Dim closingKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Return New FunctionDeclarationSyntax(functionKeyword, identifier, openParenToken, parameters, closeParenToken, type, body, endKeyword, closingKeyword)
    End Function

    Private Function ParseParameterList() As SeparatedSyntaxList(Of ParameterSyntax)

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)()

      While Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim parameter = ParseParameter()
        nodesAndSeparators.Add(parameter)

        If Current.Kind <> SyntaxKind.CloseParenToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        End If

      End While

      Return New SeparatedSyntaxList(Of ParameterSyntax)(nodesAndSeparators.ToImmutable())

    End Function

    Private Function ParseParameter() As ParameterSyntax
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim type = ParseTypeClause()
      Return New ParameterSyntax(identifier, type)
    End Function

    Private Function ParseGlobalStatement() As MemberSyntax
      Dim statement = ParseStatement()
      Return New GlobalStatementSyntax(statement)
    End Function

    Private Function ParseStatement() As StatementSyntax
      Select Case Current.Kind
        Case SyntaxKind.OpenBraceToken : Return ParseBlockStatement()
        Case SyntaxKind.DimKeyword, SyntaxKind.ConstKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.IfKeyword : Return ParseIfStatement()
        Case SyntaxKind.WhileKeyword : Return ParseWhileStatement()
        Case SyntaxKind.DoKeyword : Return ParseDoWhileStatement()
        Case SyntaxKind.ForKeyword : Return ParseForStatement()
        Case SyntaxKind.SelectKeyword : Return ParseSelectCaseStatement()
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
      Dim typeClause = ParseOptionalTypeClause()
      Dim equals = MatchToken(SyntaxKind.EqualToken)
      Dim initializer = ParseExpression()
      Return New VariableDeclarationSyntax(keyword, identifier, typeClause, equals, initializer)
    End Function

    Private Function ParseOptionalTypeClause() As TypeClauseSyntax
      If Current.Kind <> SyntaxKind.AsKeyword Then Return Nothing
      Return ParseTypeClause()
    End Function

    Private Function ParseTypeClause() As TypeClauseSyntax
      Dim asKeyword = MatchToken(SyntaxKind.AsKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Return New TypeClauseSyntax(asKeyword, identifier)
    End Function

    Private Function ParseIfStatement() As StatementSyntax

      Dim keyword = MatchToken(SyntaxKind.IfKeyword)
      Dim condition = ParseExpression()
      Dim thenKeyword = MatchToken(SyntaxKind.ThenKeyword)

      Dim thenLine = m_text.GetLineIndex(thenKeyword.Span.Start)
      Dim peekLine = m_text.GetLineIndex(Peek(0).Span.Start)
      Dim multiLine = peekLine > thenLine

      Dim statement = ParseStatement()

      If multiLine Then
        Dim ifStatement = New IfStatementSyntax(keyword, condition, thenKeyword, statement)
        'TODO: Need to handle ElseIf...
        Dim elseIfStatements = ImmutableArray(Of ElseIfStatementSyntax).Empty
        Dim elseStatement = ParseOptionalElseStatementSyntax()
        Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
        Dim closingKeyword = MatchToken(SyntaxKind.IfKeyword)
        Return New MultiLineIfBlock(ifStatement, elseIfStatements, elseStatement, endKeyword, closingKeyword)
      Else
        Dim elseClause = ParseOptionalSingleLineElseClause()
        Return New SingleLineIfStatementSyntax(keyword, condition, thenKeyword, statement, elseClause)
      End If

    End Function

    Private Function ParseCaseElseClause() As CaseElseClauseSyntax
      If Current.Kind <> SyntaxKind.CaseKeyword Then Return Nothing
      If Peek(1).Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      MatchToken(SyntaxKind.CaseKeyword)
      MatchToken(SyntaxKind.ElseKeyword)
      Dim statement = ParseStatement()
      Return New CaseElseClauseSyntax(statement)
    End Function

    Private Function ParseOptionalElseStatementSyntax() As ElseStatementSyntax
      If Current.Kind <> SyntaxKind.ElseKeyword Then
        Return Nothing
      End If
      Dim keyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statement = ParseStatement()
      Return New ElseStatementSyntax(keyword, statement)
    End Function

    Private Function ParseOptionalSingleLineElseClause() As SingleLineElseClauseSyntax
      If Current.Kind <> SyntaxKind.ElseKeyword Then
        Return Nothing
      End If
      Dim keyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statement = ParseStatement()
      Return New SingleLineElseClauseSyntax(keyword, statement)
    End Function

    Private Function ParseWhileStatement() As StatementSyntax
      Dim keyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim condition = ParseExpression()
      Dim body = ParseStatement()
      If Peek(0).Kind = SyntaxKind.WendKeyword Then
        Dim wendKeyword = MatchToken(SyntaxKind.WendKeyword)
      Else
        Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
        Dim endingWhilekeyword = MatchToken(SyntaxKind.WhileKeyword)
      End If
      Return New WhileStatementSyntax(keyword, condition, body)
    End Function

    Private Function ParseDoWhileStatement() As StatementSyntax
      Dim doKeyword = MatchToken(SyntaxKind.DoKeyword)
      Dim body = ParseStatement()
      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim condition = ParseExpression()
      Return New DoWhileStatementSyntax(doKeyword, body, whileKeyword, condition)
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

    Private Function ParseSelectCaseStatement() As StatementSyntax

      MatchToken(SyntaxKind.SelectKeyword)
      MatchToken(SyntaxKind.CaseKeyword)
      Dim test = ParseExpression()

      Dim cases = New List(Of CaseClauseSyntax)
      Do
        If Current.Kind = SyntaxKind.CaseKeyword Then
          If Peek(1).Kind = SyntaxKind.ElseKeyword Then Exit Do
          MatchToken(SyntaxKind.CaseKeyword)
          Dim matches = New List(Of CaseMatchExpressionSyntax)
          Do
            If Current.Kind = SyntaxKind.IsKeyword Then
              ' Case Is > 5
              MatchToken(SyntaxKind.IsKeyword)
              Dim comparisonKind = SyntaxKind.EqualToken
              Select Case Current.Kind
                Case SyntaxKind.LessThanToken,
                     SyntaxKind.LessThanGreaterThanToken,
                     SyntaxKind.LessThanEqualToken,
                     SyntaxKind.EqualToken,
                     SyntaxKind.GreaterThanEqualToken,
                     SyntaxKind.GreaterThanToken,
                     SyntaxKind.LessThanGreaterThanToken
                  comparisonKind = Current.Kind
                Case Else
              End Select
              MatchToken(comparisonKind)
              Dim expression = ParseExpression()
              matches.Add(New CaseMatchExpressionSyntax(comparisonKind, expression))
            Else
              ' Case 1
              ' Case variable
              ' Case 1 To 10
              Dim expression = ParseExpression()
              If Current.Kind = SyntaxKind.ToKeyword Then
                MatchToken(SyntaxKind.ToKeyword)
                Dim expressionTo = ParseExpression()
                matches.Add(New CaseMatchExpressionSyntax(SyntaxKind.EqualToken, expression, expressionTo))
              Else
                matches.Add(New CaseMatchExpressionSyntax(SyntaxKind.EqualToken, expression))
              End If
            End If
            If Current.Kind <> SyntaxKind.CommaToken Then
              Exit Do
            Else
              MatchToken(SyntaxKind.CommaToken)
            End If
          Loop
          Dim statement = ParseStatement()
          cases.Add(New CaseClauseSyntax(matches.ToImmutableArray, statement))
        Else
          Exit Do
        End If
      Loop

      Dim caseElseBlock = ParseCaseElseClause()

      MatchToken(SyntaxKind.EndKeyword)
      MatchToken(SyntaxKind.SelectKeyword)

      Return New SelectCaseStatementSyntax(test, cases.ToImmutableArray, caseElseBlock)

    End Function

    Private Function ParseExpressionStatement() As ExpressionStatementSyntax
      Dim expression = ParseExpression()
      Return New ExpressionStatementSyntax(expression)
    End Function

    Private Function IsEndOfBlock() As Boolean
      ' If/ElseIf/End If
      ' For/Next
      ' While/End While
      ' While/Wend
      ' Do/Loop
      ' Function/End Function
      ' Sub/End Sub
      ' Type/End Type
      ' Def/End Def
      ' Select Case/Case/End Select
      ' ----
      ' Struct/End Struct
      ' Try/Catch/Finally/End Try
      ' Namespace/End Namespace
      ' Module/End Module
      ' Class/End Class
      ' Interface/End Interface
      ' Enum/End Enum
      ' Using/End Using
      ' SyncLock/End SyncLock
      Select Case Current.Kind
        Case SyntaxKind.EndKeyword
          Select Case Peek(1).Kind
            Case SyntaxKind.IfKeyword,
                 SyntaxKind.WhileKeyword,
                 SyntaxKind.ForKeyword,
                 SyntaxKind.SelectKeyword,
                 SyntaxKind.FunctionKeyword
              Return True
            Case Else
              Return False
          End Select
        Case SyntaxKind.CloseBraceToken,
             SyntaxKind.ElseIfKeyword,
             SyntaxKind.WendKeyword,
             SyntaxKind.NextKeyword,
             SyntaxKind.CaseKeyword
          Return True
        Case Else
          Return False
      End Select
    End Function

    Private Function ParseBlockStatement() As BlockStatementSyntax
      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
      Dim openBraceToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.OpenBraceToken Then
        openBraceToken = MatchToken(SyntaxKind.OpenBraceToken)
      End If
      Dim startToken As SyntaxToken '= Current()
      While Current.Kind <> SyntaxKind.EndOfFileToken AndAlso Not IsEndOfBlock()
        'Current.Kind <> SyntaxKind.CloseBraceToken

        startToken = Current()
        Dim statement = ParseStatement()
        statements.Add(statement)

        ' If ParseStatement did not consume any tokens,
        ' let's skip the current token and continue in 
        ' order to avoid an infinite loop.
        '
        ' We do not need to report an error, because we've
        ' already tried to parse an expression statement
        ' and reported one.
        If Current() Is startToken Then
          NextToken()
        End If

      End While
      Dim closeBraceToken As SyntaxToken = Nothing
      If openBraceToken IsNot Nothing Then
        closeBraceToken = MatchToken(SyntaxKind.CloseBraceToken)
      End If
      Return New BlockStatementSyntax(openBraceToken, statements.ToImmutable, closeBraceToken)
    End Function

    'Private Function IntoBlock(statements As ImmutableArray(Of StatementSyntax)) As BlockStatementSyntax
    '  Dim startPosition = If(statements.Any, statements.First.Span.Start, Current.Span.Start)
    '  Dim endPosition = If(statements.Any, statements.Last.Span.Start, Current.Span.End)
    '  Dim openBraceToken = New SyntaxToken(SyntaxKind.OpenBraceToken, startPosition, "{", Nothing)
    '  Dim closeBraceToken = New SyntaxToken(SyntaxKind.CloseBraceToken, endPosition, "}", Nothing)
    '  Return New BlockStatementSyntax(openBraceToken, statements, closeBraceToken)
    'End Function

    'Private Function ParseStatements() As ImmutableArray(Of StatementSyntax)
    '  Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
    '  Dim startToken As SyntaxToken '= Current()
    '  Do

    '    Select Case Current.Kind
    '      Case SyntaxKind.EndKeyword
    '        ' End If
    '        ' End Select
    '        ' End Function
    '        ' End Sub
    '        ' End While
    '        ' End For
    '        Select Case Peek(1).Kind
    '          Case SyntaxKind.IfKeyword : Exit Do
    '          Case SyntaxKind.SelectKeyword : Exit Do
    '          Case SyntaxKind.FunctionKeyword : Exit Do
    '          'Case SyntaxKind.SubKeyword : Exit Do
    '          Case SyntaxKind.WhileKeyword : Exit Do
    '          Case SyntaxKind.ForKeyword : Exit Do
    '          Case Else
    '        End Select
    '      Case SyntaxKind.CloseBraceToken,
    '           SyntaxKind.ElseKeyword,
    '           SyntaxKind.ElseIfKeyword,
    '           SyntaxKind.NextKeyword,
    '           SyntaxKind.CaseKeyword,
    '           SyntaxKind.EndOfFileToken
    '        Exit Do
    '      Case Else
    '    End Select

    '    startToken = Current()
    '    Dim statement = ParseStatement()
    '    statements.Add(statement)

    '    ' If ParseStatement did not consume any tokens,
    '    ' let's skip the current token and continue in 
    '    ' order to avoid an infinite loop.
    '    '
    '    ' We do not need to report an error, because we've
    '    ' already tried to parse an expression statement
    '    ' and reported one.
    '    If Current() Is startToken Then
    '      NextToken()
    '    End If

    '  Loop
    '  Return statements.ToImmutable
    'End Function

    Private Function ParseExpression() As ExpressionSyntax
      Return ParseAssignmentExpression()
    End Function

    Private Function ParseAssignmentExpression() As ExpressionSyntax

      If SyntaxFacts.GetKeywordKind(Current.Text) = SyntaxKind.LetKeyword Then
        Dim letKeyword = NextToken()
      End If

      If Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
         Peek(1).Kind = SyntaxKind.EqualToken Then
        Dim identifierToken = NextToken()
        Dim operatorToken = NextToken()
        Dim right = ParseAssignmentExpression()
        Return New AssignmentExpressionSyntax(identifierToken, operatorToken, right)
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
        Case SyntaxKind.StringToken : Return ParseStringLiteral()
        Case SyntaxKind.IdentifierToken : Return ParseNameOrCallExpression()
        Case Else : Return ParseNameOrCallExpression() 'Case SyntaxKind.IdentifierToken
      End Select
    End Function

    Private Function ParseNameOrCallExpression() As ExpressionSyntax
      If Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
         Peek(1).Kind = SyntaxKind.OpenParenToken Then
        Return ParseCallExpression()
      End If
      Return ParseNameExpression()
    End Function

    Private Function ParseCallExpression() As ExpressionSyntax
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim arguments = ParseArguments()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Return New CallExpressionSyntax(identifier, openParenToken, arguments, closeParenToken)
    End Function

    Private Function ParseArguments() As SeparatedSyntaxList(Of ExpressionSyntax)
      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)
      Dim parseNextArgument = True
      While parseNextArgument AndAlso
            Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken
        Dim expression = ParseExpression()
        nodesAndSeparators.Add(expression)
        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextArgument = False
        End If
      End While
      Return New SeparatedSyntaxList(Of ExpressionSyntax)(nodesAndSeparators.ToImmutable)
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

    Private Function ParseStringLiteral() As ExpressionSyntax
      Dim stringToken = MatchToken(SyntaxKind.StringToken)
      Return New LiteralExpressionSyntax(stringToken)
    End Function

    Private Function ParseNameExpression() As ExpressionSyntax
      Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
      Return New NameExpressionSyntax(identifierToken)
    End Function

  End Class

End Namespace
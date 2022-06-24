Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Parser

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_syntaxTree As SyntaxTree
    Private ReadOnly m_text As SourceText
    Private ReadOnly m_tokens As ImmutableArray(Of SyntaxToken)

    Private m_position As Integer

    Public Sub New(tree As SyntaxTree) 'text As SourceText)

      Dim tokens = New List(Of SyntaxToken)
      Dim badTokens = New List(Of SyntaxToken)
      Dim lexer = New Lexer(tree)
      Dim token As SyntaxToken
      'Do

      '  token = lexer.Lex

      '  If token.Kind <> SyntaxKind.WhitespaceToken AndAlso token.Kind <> SyntaxKind.BadToken Then
      '    tokens.Add(token)
      '  End If

      'Loop While token.Kind <> SyntaxKind.EndOfFileToken

      'm_text = text
      'm_tokens = tokens.ToImmutableArray
      'm_diagnostics.AddRange(lexer.Diagnostics)

      Do

        token = lexer.Lex

        If token.Kind = SyntaxKind.BadToken Then
          badTokens.Add(token)
        Else
          If badTokens.Count > 0 Then
            Dim leadingTrivia = token.LeadingTrivia.ToBuilder
            Dim index = 0
            For Each badToken In badTokens
              For Each lt In badToken.LeadingTrivia
                leadingTrivia.Insert(index, lt) : index += 1
              Next
              Dim trivia = New SyntaxTrivia(tree, SyntaxKind.SkippedTextTrivia, badToken.Position, badToken.Text)
              leadingTrivia.Insert(index, trivia) : index += 1
              For Each tt In badToken.TrailingTrivia
                leadingTrivia.Insert(index, tt) : index += 1
              Next
            Next
            badTokens.Clear()
            token = New SyntaxToken(token.SyntaxTree, token.Kind, token.Position, token.Text, token.Value, leadingTrivia.ToImmutable, token.TrailingTrivia)
          End If
          tokens.Add(token)
        End If

      Loop While token.Kind <> SyntaxKind.EndOfFileToken
      m_syntaxTree = tree
      m_text = tree.Text
      m_diagnostics.AddRange(lexer.Diagnostics)
      m_tokens = tokens.ToImmutableArray

    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag
      Get
        Return m_diagnostics
      End Get
    End Property

    Public Function ParseCompilationUnit() As CompilationUnitSyntax
      Dim members = ParseMembers()
      Dim endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken)
      Return New CompilationUnitSyntax(m_syntaxTree, members, endOfFileToken)
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

    Private Function ParseGlobalStatement() As MemberSyntax
      Dim statement = ParseStatement()
      Return New GlobalStatementSyntax(m_syntaxTree, statement)
    End Function

    Private Function ParseStatement() As StatementSyntax
      Select Case Current.Kind
        Case SyntaxKind.ConstKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.ContinueKeyword : Return ParseContinueStatement()
        Case SyntaxKind.DimKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.DoKeyword : Return ParseDoStatement()
        Case SyntaxKind.EndKeyword : Return ParseEndStatement()
        Case SyntaxKind.ExitKeyword : Return ParseExitStatement()
        Case SyntaxKind.ForKeyword : Return ParseForStatement()
        Case SyntaxKind.GotoKeyword : Return ParseGotoStatement()
        Case SyntaxKind.IfKeyword : Return ParseIfStatement()
        Case SyntaxKind.Label : Return ParseLabelStatement()
        Case SyntaxKind.PrintKeyword : Return ParsePrintStatement()
        Case SyntaxKind.OpenBraceToken : Return ParseBlockStatement()
        Case SyntaxKind.ReturnKeyword : Return ParseReturnStatement()
        Case SyntaxKind.WhileKeyword : Return ParseWhileStatement()
        Case Else : Return ParseExpressionStatement()
      End Select
    End Function

    Private Function ParsePrintStatement() As PrintStatementSyntax

      ' PRINT [#*file_number*,][*output_list*][{;|,}]

      Dim printKeyword = MatchToken(SyntaxKind.PrintKeyword)

      Dim nodes = ImmutableArray.CreateBuilder(Of SyntaxNode)()

      Dim printLine = m_text.GetLineIndex(printKeyword.Span.Start)

      If Current.Kind = SyntaxKind.PoundToken Then
        Dim poundToken = MatchToken(SyntaxKind.PoundToken)
        Dim fileNumberExpression = ParseExpression()
        Dim commaToken = MatchToken(SyntaxKind.CommaToken)
        nodes.Add(poundToken)
        nodes.Add(fileNumberExpression)
        nodes.Add(commaToken)
      End If

      'Dim lastToken = SyntaxKind.BadToken
      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        If currentLine <> printLine Then Exit While

        Dim expression = ParseExpression()
        If expression IsNot Nothing Then
          'If lastToken = SyntaxKind.ExpressionStatement Then
          '  Dim semiColonToken = New SyntaxToken(m_syntaxTree, SyntaxKind.SemicolonToken, expression.Span.Start, ";", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
          '  nodes.Add(semiColonToken)
          'End If
          nodes.Add(expression)
          'lastToken = SyntaxKind.ExpressionStatement
        End If

        If Current.Kind = SyntaxKind.SpcKeyword Then
          Dim spcKeyword = MatchToken(SyntaxKind.SpcKeyword)
          Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
          Dim valueExpression = ParseExpression()
          Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
          Dim spcFunction = New SpcFunctionSyntax(m_syntaxTree, spcKeyword, openParen, valueExpression, closeParen)
          nodes.Add(spcFunction)
        ElseIf Current.Kind = SyntaxKind.TabKeyword Then
          Dim tabKeyword = MatchToken(SyntaxKind.TabKeyword)
          Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
          Dim valueExpression = ParseExpression()
          Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
          Dim tabFunction = New TabFunctionSyntax(m_syntaxTree, tabKeyword, openParen, valueExpression, closeParen)
          nodes.Add(tabFunction)
        ElseIf Current.Kind = SyntaxKind.CommaToken Then
          Dim commaToken = MatchToken(SyntaxKind.CommaToken)
          nodes.Add(commaToken)
          'lastToken = SyntaxKind.CommaToken
        ElseIf Current.Kind = SyntaxKind.SemicolonToken Then
          Dim semiColonToken = MatchToken(SyntaxKind.SemicolonToken)
          'If lastToken <> SyntaxKind.SemicolonToken Then
          nodes.Add(semiColonToken)
          'End If
          'lastToken = SyntaxKind.SemicolonToken
        End If

      End While

      Return New PrintStatementSyntax(m_syntaxTree, printKeyword, If(nodes IsNot Nothing, nodes.ToImmutable, Nothing))

    End Function

    Private Function ParseGotoStatement() As GotoStatementSyntax

      ' GOTO NumberToken

      ' GOTO IdentifierToken

      Dim gotoKeyword = MatchToken(SyntaxKind.GotoKeyword)

      If Current.Kind = SyntaxKind.NumberToken Then
        Dim numberToken = MatchToken(SyntaxKind.NumberToken)
        Return New GotoStatementSyntax(m_syntaxTree, gotoKeyword, numberToken)
      Else
        Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
        Return New GotoStatementSyntax(m_syntaxTree, gotoKeyword, identifierToken)
      End If

    End Function

    Private Function ParseVariableDeclaration() As StatementSyntax

      ' DIM *identifier* [AS *type*] [= *initializer*]
      ' CONST *identifier* [AS *type*] [= *initializer*]

      Dim expected = SyntaxKind.DimKeyword
      If Current.Kind = SyntaxKind.ConstKeyword Then expected = SyntaxKind.ConstKeyword
      If Current.Kind = SyntaxKind.DimKeyword Then expected = SyntaxKind.DimKeyword
      Dim keyword = MatchToken(expected)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim asClause = ParseOptionalAsClause()
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim initializer = ParseExpression()
      Return New VariableDeclarationSyntax(m_syntaxTree, keyword, identifier, asClause, equalToken, initializer)

    End Function

    Private Function ParseOptionalAsClause() As AsClauseSyntax
      If Current.Kind <> SyntaxKind.AsKeyword Then Return Nothing
      Return ParseAsClause()
    End Function

    Private Function ParseAsClause() As AsClauseSyntax

      ' ... AS *type*

      Dim asKeyword = MatchToken(SyntaxKind.AsKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Return New AsClauseSyntax(m_syntaxTree, asKeyword, identifier)

    End Function

#Region "Function Block"

    Private Function ParseFunctionDeclaration() As MemberSyntax

      ' FUNCTION *identifier*([*parameters*])[ AS *type*]
      '   *statements*
      ' END FUNCTION

      Dim functionKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim parameters = ParseParameterList()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Dim asClause = ParseOptionalAsClause()
      Dim statements = ParseBlockStatement()
      Dim endFunctionKeyword = MatchToken(SyntaxKind.EndFunctionKeyword)
      Return New FunctionDeclarationSyntax(m_syntaxTree, functionKeyword, identifier, openParenToken, parameters, closeParenToken, asClause, statements, endFunctionKeyword)

    End Function

    Private Function ParseParameterList() As SeparatedSyntaxList(Of ParameterSyntax)

      ' ...(... *identifier* AS *type*[, *identifier* AS *type*] ...)...

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)()

      Dim parseNextParameter = True
      While parseNextParameter AndAlso
            Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim parameter = ParseParameter()
        nodesAndSeparators.Add(parameter)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextParameter = False
        End If

      End While

      Return New SeparatedSyntaxList(Of ParameterSyntax)(nodesAndSeparators.ToImmutable())

    End Function

    Private Function ParseParameter() As ParameterSyntax

      ' ... *identifier* AS *type* ...

      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim asClause = ParseAsClause()
      Return New ParameterSyntax(m_syntaxTree, identifier, asClause)

    End Function

#End Region

#Region "Flow Control Blocks"

    Private Function ParseReturnStatement() As StatementSyntax

      ' RETURN [*expression*]

      Dim returnKeyword = MatchToken(SyntaxKind.ReturnKeyword)
      Dim keywordLine = m_text.GetLineIndex(returnKeyword.Span.Start)
      Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
      Dim isEof = Current.Kind = SyntaxKind.EndOfFileToken
      Dim sameLine = Not isEof AndAlso keywordLine = currentLine
      Dim expression = If(sameLine, ParseExpression(), Nothing)
      Return New ReturnStatementSyntax(m_syntaxTree, returnKeyword, expression)

    End Function

    Private Function ParseLabelStatement() As StatementSyntax

      ' DoSomething:

      Dim label = MatchToken(SyntaxKind.Label)
      Return New LabelStatementSyntax(m_syntaxTree, label)

    End Function

    Private Function ParseEndStatement() As StatementSyntax

      ' End

      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Return New EndStatementSyntax(m_syntaxTree, endKeyword)

    End Function

    Private Function ParseExitStatement() As StatementSyntax

      ' Exit Do
      ' Exit For
      ' Exit Function
      ' Exit Sub
      ' Exit While

      Dim exitKeyword = MatchToken(SyntaxKind.ExitKeyword)
      Dim kind As SyntaxKind = SyntaxKind.ForKeyword
      Select Case Current.Kind
        Case SyntaxKind.DoKeyword,
             SyntaxKind.ForKeyword,
             SyntaxKind.FunctionKeyword,
             SyntaxKind.WhileKeyword
          kind = Current.Kind
        Case Else
      End Select
      Dim scopeKeyword = MatchToken(kind)
      Return New ExitStatementSyntax(m_syntaxTree, exitKeyword, scopeKeyword)

    End Function

    Private Function ParseContinueStatement() As StatementSyntax

      ' Continue Do
      ' Continue For
      ' Continue While

      Dim continueKeyword = MatchToken(SyntaxKind.ContinueKeyword)
      Dim kind As SyntaxKind = SyntaxKind.ForKeyword
      Select Case Current.Kind
        Case SyntaxKind.DoKeyword,
             SyntaxKind.ForKeyword,
             SyntaxKind.WhileKeyword
          kind = Current.Kind
        Case Else
      End Select
      Dim scopeKeyword = MatchToken(kind)
      Return New ContinueStatementSyntax(m_syntaxTree, continueKeyword, scopeKeyword)

    End Function

    Private Function ParseDoStatement() As StatementSyntax

      ' DO [WHILE *boolean_expression*]
      '   *statements*
      ' LOOP [WHILE *boolean_expression*]

      ' or

      ' DO [UNTIL *boolean_expression*]
      '   *statements*
      ' LOOP [UNTIL *boolean_expression*]

      Dim whileClause As WhileClauseSyntax = Nothing
      Dim untilClause As UntilClauseSyntax = Nothing

      Dim doKeyword = MatchToken(SyntaxKind.DoKeyword)

      Select Case Current.Kind
        Case SyntaxKind.WhileKeyword : whileClause = ParseOptionalWhileClause(True)
        Case SyntaxKind.UntilKeyword : untilClause = ParseOptionalUntilClause(True)
        Case Else
      End Select

      Dim body = ParseBlockStatement()

      Dim loopKeyword = MatchToken(SyntaxKind.LoopKeyword)

      If whileClause Is Nothing AndAlso untilClause Is Nothing Then
        Select Case Current.Kind
          Case SyntaxKind.WhileKeyword : whileClause = ParseOptionalWhileClause(False)
          Case SyntaxKind.UntilKeyword : untilClause = ParseOptionalUntilClause(False)
          Case Else
        End Select
      End If

      If untilClause Is Nothing Then
        Return New DoWhileStatementSyntax(m_syntaxTree, doKeyword, whileClause, body, loopKeyword)
      Else
        Return New DoUntilStatementSyntax(m_syntaxTree, doKeyword, untilClause, body, loopKeyword)
      End If

    End Function

    Private Function ParseOptionalUntilClause(atBeginning As Boolean) As UntilClauseSyntax

      ' ... UNTIL *expression*

      If Current.Kind <> SyntaxKind.UntilKeyword Then Return Nothing
      Dim untilKeyword = MatchToken(SyntaxKind.UntilKeyword)
      Dim expression = ParseExpression()
      Return New UntilClauseSyntax(m_syntaxTree, untilKeyword, expression, atBeginning)

    End Function

    Private Function ParseOptionalWhileClause(atBeginning As Boolean) As WhileClauseSyntax

      ' ... WHILE *expression*

      If Current.Kind <> SyntaxKind.WhileKeyword Then Return Nothing
      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim expression = ParseExpression()
      Return New WhileClauseSyntax(m_syntaxTree, whileKeyword, expression, atBeginning)

    End Function

    Private Function ParseForStatement() As StatementSyntax

      If Current.Kind = SyntaxKind.ForKeyword AndAlso Peek(1).Kind = SyntaxKind.EachKeyword Then
        Return ParseForEachStatement()
      End If

      ' FOR *identifier* = *start_value* TO *end_value* [STEP *increment*]
      '   *statements*
      ' NEXT [*identifier* [, *identifier*]...]

      Dim forKeyword = MatchToken(SyntaxKind.ForKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim startValue = ParseExpression()
      Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
      Dim endValue = ParseExpression()
      Dim stepKeyword As SyntaxToken = Nothing
      Dim increment As ExpressionSyntax = Nothing
      If Peek(0).Kind = SyntaxKind.StepKeyword Then
        stepKeyword = MatchToken(SyntaxKind.StepKeyword)
        increment = ParseExpression()
      End If
      Dim statements = ParseBlockStatement()
      Dim nextKeyword = MatchToken(SyntaxKind.NextKeyword)
      Return New ForStatementSyntax(m_syntaxTree, forKeyword,
                                    identifier,
                                    equalToken,
                                    startValue,
                                    toKeyword,
                                    endValue,
                                    stepKeyword,
                                    increment,
                                    statements,
                                    nextKeyword)
    End Function

    Private Function ParseForEachStatement() As StatementSyntax

      ' FOR EACH *value* IN *array*
      '   *statements*
      ' NEXT

      Dim forKeyword = MatchToken(SyntaxKind.ForKeyword)
      Dim eachKeyword = MatchToken(SyntaxKind.EachKeyword)
      Dim value = MatchToken(SyntaxKind.IdentifierToken)
      Dim inKeyword = MatchToken(SyntaxKind.InKeyword)
      Dim array = MatchToken(SyntaxKind.IdentifierToken)
      Dim statements = ParseBlockStatement()
      Dim nextKeyword = MatchToken(SyntaxKind.NextKeyword)
      Return New ForEachStatementSyntax(m_syntaxTree, forKeyword,
                                        eachKeyword,
                                        value,
                                        inKeyword,
                                        array,
                                        statements,
                                        nextKeyword)
    End Function

    Private Function ParseWhileStatement() As StatementSyntax

      ' WHILE [*boolean_expression*]
      '   *statements*
      ' WEND

      ' or

      ' WHILE [*boolean_expression*]
      '   *statements*
      ' END WHILE

      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim expression = ParseExpression()
      Dim body = ParseBlockStatement()
      'If Peek(0).Kind = SyntaxKind.WendKeyword Then
      Dim wendKeyword = MatchToken(SyntaxKind.WendKeyword)
      'Else
      '  Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      '  Dim endingWhilekeyword = MatchToken(SyntaxKind.WhileKeyword)
      'End If
      'TODO: Need to decide how to handle WEND and END WHILE...
      Return New WhileStatementSyntax(m_syntaxTree, whileKeyword, expression, body, wendKeyword)

    End Function

#End Region

#Region "Decision Blocks"

    Private Function ParseIfStatement() As StatementSyntax

      ' IF *expression* THEN
      '   *statements*
      ' [ELSEIF *expression*
      '   *statements*]
      ' [ELSE
      '   *statements*
      ' END IF

      ' or

      ' IF *expression* THEN *statements* [ELSE [*statements*]]

      Dim ifKeyword = MatchToken(SyntaxKind.IfKeyword)
      Dim expression = ParseExpression()
      Dim thenKeyword = MatchToken(SyntaxKind.ThenKeyword)

      Dim thenLine = m_text.GetLineIndex(thenKeyword.Span.Start)
      Dim peekLine = m_text.GetLineIndex(Peek(0).Span.Start)
      Dim multiLine = Current.Kind = SyntaxKind.EndOfFileToken OrElse peekLine > thenLine

      Dim statements = ParseBlockStatement()

      If multiLine Then
        'Dim ifStatement = New IfStatementSyntax(m_syntaxTree, ifKeyword, expression, thenKeyword, statements)
        'TODO: Need to handle ElseIf...
        'Dim elseIfStatements = ImmutableArray(Of ElseIfStatementSyntax).Empty
        Dim elseClause = ParseOptionalElseClauseSyntax()
        Dim endIfKeyword = MatchToken(SyntaxKind.EndIfKeyword)
        Return New IfStatementSyntax(m_syntaxTree,
                                     ifKeyword,
                                     expression,
                                     thenKeyword,
                                     statements,
                                     elseClause,
                                     endIfKeyword)
      Else
        Dim elseClause = ParseOptionalSingleLineElseClause()
        Return New SingleLineIfStatementSyntax(m_syntaxTree,
                                               ifKeyword,
                                               expression,
                                               thenKeyword,
                                               statements,
                                               elseClause)
      End If

    End Function

    Private Function ParseOptionalSingleLineElseClause() As SingleLineElseClauseSyntax

      ' ... ELSE *statements*

      If Current.Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseBlockStatement()
      Return New SingleLineElseClauseSyntax(m_syntaxTree, elseKeyword, statements)

    End Function

    Private Function ParseOptionalElseClauseSyntax() As ElseClauseSyntax

      ' ...
      ' ELSE
      '   *statements*
      ' ...

      If Current.Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseBlockStatement()
      Return New ElseClauseSyntax(m_syntaxTree, elseKeyword, statements)

    End Function

    'Private Function ParseSelectCaseStatement() As StatementSyntax

    '  MatchToken(SyntaxKind.SelectKeyword)
    '  MatchToken(SyntaxKind.CaseKeyword)
    '  Dim test = ParseExpression()

    '  Dim cases = New List(Of CaseClauseSyntax)
    '  Do
    '    If Current.Kind = SyntaxKind.CaseKeyword Then
    '      If Peek(1).Kind = SyntaxKind.ElseKeyword Then Exit Do
    '      MatchToken(SyntaxKind.CaseKeyword)
    '      Dim matches = New List(Of CaseMatchExpressionSyntax)
    '      Do
    '        If Current.Kind = SyntaxKind.IsKeyword Then
    '          ' Case Is > 5
    '          MatchToken(SyntaxKind.IsKeyword)
    '          Dim comparisonKind = SyntaxKind.EqualToken
    '          Select Case Current.Kind
    '            Case SyntaxKind.LessThanToken,
    '                 SyntaxKind.LessThanGreaterThanToken,
    '                 SyntaxKind.LessThanEqualToken,
    '                 SyntaxKind.EqualToken,
    '                 SyntaxKind.GreaterThanEqualToken,
    '                 SyntaxKind.GreaterThanToken,
    '                 SyntaxKind.LessThanGreaterThanToken
    '              comparisonKind = Current.Kind
    '            Case Else
    '          End Select
    '          MatchToken(comparisonKind)
    '          Dim expression = ParseExpression()
    '          matches.Add(New CaseMatchExpressionSyntax(comparisonKind, expression))
    '        Else
    '          ' Case 1
    '          ' Case variable
    '          ' Case 1 To 10
    '          Dim expression = ParseExpression()
    '          If Current.Kind = SyntaxKind.ToKeyword Then
    '            MatchToken(SyntaxKind.ToKeyword)
    '            Dim expressionTo = ParseExpression()
    '            matches.Add(New CaseMatchExpressionSyntax(SyntaxKind.EqualToken, expression, expressionTo))
    '          Else
    '            matches.Add(New CaseMatchExpressionSyntax(SyntaxKind.EqualToken, expression))
    '          End If
    '        End If
    '        If Current.Kind <> SyntaxKind.CommaToken Then
    '          Exit Do
    '        Else
    '          MatchToken(SyntaxKind.CommaToken)
    '        End If
    '      Loop
    '      Dim statement = ParseBlockStatement() 'ParseStatement()
    '      cases.Add(New CaseClauseSyntax(matches.ToImmutableArray, statement))
    '    Else
    '      Exit Do
    '    End If
    '  Loop

    '  Dim caseElseBlock = ParseCaseElseClause()

    '  MatchToken(SyntaxKind.EndKeyword)
    '  MatchToken(SyntaxKind.SelectKeyword)

    '  Return New SelectCaseStatementSyntax(test, cases.ToImmutableArray, caseElseBlock)

    'End Function

    'Private Function ParseCaseElseClause() As CaseElseClauseSyntax
    '  If Current.Kind <> SyntaxKind.CaseKeyword Then Return Nothing
    '  If Peek(1).Kind <> SyntaxKind.ElseKeyword Then Return Nothing
    '  MatchToken(SyntaxKind.CaseKeyword)
    '  MatchToken(SyntaxKind.ElseKeyword)
    '  Dim statement = ParseBlockStatement() 'ParseStatement()
    '  Return New CaseElseClauseSyntax(statement)
    'End Function

#End Region

    Private Function ParseExpressionStatement() As ExpressionStatementSyntax
      Dim expression = ParseExpression()
      Return New ExpressionStatementSyntax(m_syntaxTree, expression)
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
        Case SyntaxKind.EndIfKeyword,
             SyntaxKind.EndFunctionKeyword
          'Select Case Peek(1).Kind
          '  Case SyntaxKind.IfKeyword,
          '       SyntaxKind.WhileKeyword,
          '       SyntaxKind.ForKeyword,
          '       SyntaxKind.FunctionKeyword
          Return True
          '  Case Else
          '    Return False
          'End Select
        Case SyntaxKind.CloseBraceToken,
             SyntaxKind.ElseKeyword,
             SyntaxKind.ElseIfKeyword,
             SyntaxKind.WendKeyword,
             SyntaxKind.NextKeyword,
             SyntaxKind.LoopKeyword
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
      Return New BlockStatementSyntax(m_syntaxTree, openBraceToken, statements.ToImmutable, closeBraceToken)
    End Function

    Private Function ParseExpression() As ExpressionSyntax
      Return ParseAssignmentExpression()
    End Function

    Private Function ParseAssignmentExpression() As ExpressionSyntax

      ' LET *identifier* = *expression*

      ' or

      ' *identifier* = *expression*

      'TODO: Need to decide how to allow for LET being optional and it not conflicting with binary expressions.

      'If SyntaxFacts.GetKeywordKind(Current.Text) = SyntaxKind.LetKeyword Then
      '  Dim letKeyword = NextToken()
      'End If

      If Current.Kind = SyntaxKind.LetKeyword Then 'SyntaxFacts.GetKeywordKind(Current.Text) = SyntaxKind.LetKeyword Then
        Dim letKeyword = MatchToken(SyntaxKind.LetKeyword)
        'If Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
        '   Peek(1).Kind = SyntaxKind.EqualToken Then
        Dim identifierToken = NextToken()
        Dim operatorToken = MatchToken(SyntaxKind.EqualToken)
        Dim right = ParseAssignmentExpression()
        Return New AssignmentExpressionSyntax(m_syntaxTree, letKeyword, identifierToken, operatorToken, right)
        'End If
      End If

      Return ParseBinaryExpression()

    End Function

    Private Function ParseBinaryExpression(Optional parentPrecedence As Integer = 0) As ExpressionSyntax
      Dim left As ExpressionSyntax
      Dim unaryOperatorPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(Current.Kind)
      If unaryOperatorPrecedence <> 0 AndAlso unaryOperatorPrecedence > parentPrecedence Then
        Dim operatorToken = NextToken()
        Dim operand = ParseBinaryExpression(unaryOperatorPrecedence)
        left = New UnaryExpressionSyntax(m_syntaxTree, operatorToken, operand)
      Else
        left = ParsePrimaryExpression()
      End If
      Do
        Dim precedence = SyntaxFacts.GetBinaryOperatorPrecedence(Current.Kind)
        If precedence = 0 OrElse precedence <= parentPrecedence Then Exit Do
        Dim operatorToken = NextToken()
        Dim right = ParseBinaryExpression(precedence)
        left = New BinaryExpressionSyntax(m_syntaxTree, left, operatorToken, right)
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

      ' *identifier*(*arguments*)

      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim arguments = ParseArguments()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Return New CallExpressionSyntax(m_syntaxTree, identifier, openParenToken, arguments, closeParenToken)

    End Function

    Private Function ParseArguments() As SeparatedSyntaxList(Of ExpressionSyntax)

      ' ... *expression*[, *expression*] ...

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

      ' ... (*expression*) ...

      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim expression = ParseExpression()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Return New ParenExpressionSyntax(m_syntaxTree, openParenToken, expression, closeParenToken)

    End Function

    Private Function ParseBooleanLiteral() As ExpressionSyntax

      ' ... true ...

      ' or

      ' ... false ...

      Dim isTrue = (Current.Kind = SyntaxKind.TrueKeyword)
      Dim keywordToken = MatchToken(If(isTrue, SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword))
      Return New LiteralExpressionSyntax(m_syntaxTree, keywordToken, isTrue)

    End Function

    Private Function ParseNumberLiteral() As ExpressionSyntax

      ' ... *number* ...

      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(m_syntaxTree, numberToken)

    End Function

    Private Function ParseStringLiteral() As ExpressionSyntax

      ' ... "*string*" ...

      Dim stringToken = MatchToken(SyntaxKind.StringToken)
      Return New LiteralExpressionSyntax(m_syntaxTree, stringToken)

    End Function

    Private Function ParseNameExpression() As ExpressionSyntax

      ' ... *identifier* ...

      Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
      Return New NameExpressionSyntax(m_syntaxTree, identifierToken)

    End Function

#Region "Internal Support Methods"

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
      m_diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, kind)
      Return New SyntaxToken(m_syntaxTree, kind, Current.Position, Nothing, Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
    End Function

#End Region

  End Class

End Namespace
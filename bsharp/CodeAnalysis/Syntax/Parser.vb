Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Text

Namespace Bsharp.CodeAnalysis.Syntax

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
      Dim statement = ParseStatement(True)
      Return New GlobalStatementSyntax(m_syntaxTree, statement)
    End Function

    Private Function ParseStatement(isTopLevel As Boolean) As StatementSyntax

      Select Case Current.Kind
        'Case SyntaxKind.ChDirKeyword
        Case SyntaxKind.ChDirKeyword : Return ParseChDirStatement()
        Case SyntaxKind.ClearKeyword : Return ParseClearStatement()
        Case SyntaxKind.ClsKeyword : Return ParseClsStatement()
        'Case SyntaxKind.ColorKeyword
        Case SyntaxKind.ConstKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.ContinueKeyword : Return ParseContinueStatement()
        Case SyntaxKind.DataKeyword : Return ParseDataStatement()
        Case SyntaxKind.DimKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.DoKeyword : Return ParseDoStatement(isTopLevel)
        Case SyntaxKind.EndKeyword : Return ParseEndStatement()
        Case SyntaxKind.ExitKeyword : Return ParseExitStatement()
        Case SyntaxKind.ForKeyword : Return ParseForStatement(isTopLevel)
        Case SyntaxKind.GotoKeyword : Return ParseGotoStatement()
        Case SyntaxKind.GosubKeyword : Return ParseGosubStatement()
        Case SyntaxKind.IfKeyword : Return ParseIfStatement(isTopLevel)
        Case SyntaxKind.InputKeyword : Return ParseInputStatement()
        Case SyntaxKind.KillKeyword : Return ParseKillStatement()
        Case SyntaxKind.Label : Return ParseLabelStatement()
        Case SyntaxKind.LetKeyword : Return ParseLetStatement()
        Case SyntaxKind.MkDirKeyword : Return ParseMkDirStatement()
        Case SyntaxKind.MidKeyword : Return ParseMidStatement()
        Case SyntaxKind.NameKeyword : Return ParseNameStatement()
        Case SyntaxKind.PrintKeyword : Return ParsePrintStatement()
        Case SyntaxKind.OpenBraceToken : Return ParseBlockStatement(isTopLevel)
        Case SyntaxKind.OptionKeyword : Return ParseOptionStatement()
        Case SyntaxKind.ReadKeyword : Return ParseReadStatement()
        Case SyntaxKind.RemKeyword : Return ParseRemStatement()
        Case SyntaxKind.RestoreKeyword : Return ParseRestoreStatement()
        Case SyntaxKind.ReturnKeyword : Return ParseReturnStatement(isTopLevel)
        Case SyntaxKind.RmDirKeyword : Return ParseRmDirStatement()
        Case SyntaxKind.StopKeyword : Return ParseStopStatement()
        Case SyntaxKind.SystemKeyword : Return ParseSystemStatement()
        Case SyntaxKind.WhileKeyword : Return ParseWhileStatement(isTopLevel)
        Case Else
          If Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
             Peek(1).Kind = SyntaxKind.EqualToken Then

            ' *identifier* = *expression*

            Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
            Dim equalsToken = MatchToken(SyntaxKind.EqualToken)
            Dim expression = ParseExpression()
            Return New ExpressionStatementSyntax(m_syntaxTree, New AssignmentExpressionSyntax(m_syntaxTree, identifierToken, equalsToken, expression))

          End If

          Return ParseExpressionStatement()

      End Select

    End Function

    Private Function ParseChDirStatement() As ChDirStatementSyntax

      ' CHDIR path

      Dim chDirKeyword = MatchToken(SyntaxKind.ChDirKeyword)
      Dim path = ParseExpression()
      Return New ChDirStatementSyntax(m_syntaxTree, chDirKeyword, path)

    End Function

    Private Function ParseClearStatement() As ClearStatementSyntax

      ' CLEAR[,[expression1][,expression2]]

      Dim clearKeyword = MatchToken(SyntaxKind.ClearKeyword)
      Dim maxBytesCommaToken As SyntaxToken = Nothing
      Dim stackSpaceCommaToken As SyntaxToken = Nothing
      Dim maxBytesExpression As ExpressionSyntax = Nothing
      Dim stackSpaceExpression As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        maxBytesCommaToken = MatchToken(SyntaxKind.CommaToken)
      End If
      If maxBytesCommaToken IsNot Nothing Then
        If IsPossibleExpression() Then
          maxBytesExpression = ParseExpression()
        End If
        If Current.Kind = SyntaxKind.CommaToken Then
          stackSpaceCommaToken = MatchToken(SyntaxKind.CommaToken)
          stackSpaceExpression = ParseExpression()
        End If
      End If
      Return New ClearStatementSyntax(m_syntaxTree, clearKeyword, maxBytesCommaToken, maxBytesExpression, stackSpaceCommaToken, stackSpaceExpression)

    End Function

    Private Function ParseClsStatement() As ClsStatementSyntax

      ' CLS [*expression*]

      Dim clsKeyword = MatchToken(SyntaxKind.ClsKeyword)
      Dim expression As ExpressionSyntax = Nothing
      If IsPossibleExpression() Then
        expression = ParseExpression()
      End If
      Return New ClsStatementSyntax(m_syntaxTree, clsKeyword, expression)

    End Function

    Private Function ParseDataStatement() As DataStatementSyntax

      ' DATA constant[,constant]...

      Dim dataKeyword = MatchToken(SyntaxKind.DataKeyword)

      Dim constantsAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxToken)()

      Dim parseNextConstant = True
      While parseNextConstant AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim kind = SyntaxKind.NumberToken
        If Current.Kind = SyntaxKind.StringToken Then
          kind = Current.Kind
        End If
        Dim constant = MatchToken(kind)
        constantsAndSeparators.Add(constant)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          constantsAndSeparators.Add(comma)
        Else
          parseNextConstant = False
        End If

      End While

      Return New DataStatementSyntax(m_syntaxTree, dataKeyword, constantsAndSeparators.ToImmutable)

    End Function

    Private Function ParseGosubStatement() As GosubStatementSyntax

      ' GOSUB *line number*

      ' GOSUB *label*

      Dim gosubKeyword = MatchToken(SyntaxKind.GosubKeyword)

      If Current.Kind = SyntaxKind.NumberToken Then
        Dim numberToken = MatchToken(SyntaxKind.NumberToken)
        Return New GosubStatementSyntax(m_syntaxTree, gosubKeyword, numberToken)
      Else
        Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
        Return New GosubStatementSyntax(m_syntaxTree, gosubKeyword, identifierToken)
      End If

    End Function

    Private Function ParseGotoStatement() As GotoStatementSyntax

      ' GOTO *line number*

      ' GOTO *label*

      Dim gotoKeyword = MatchToken(SyntaxKind.GotoKeyword)

      If Current.Kind = SyntaxKind.NumberToken Then
        Dim numberToken = MatchToken(SyntaxKind.NumberToken)
        Return New GotoStatementSyntax(m_syntaxTree, gotoKeyword, numberToken)
      Else
        Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
        Return New GotoStatementSyntax(m_syntaxTree, gotoKeyword, identifierToken)
      End If

    End Function

    Private Function ParseInputStatement() As InputStatementSyntax

      ' INPUT[;][prompt string;] list of variables
      ' INPUT[;][prompt String,] list Of variables

      Dim inputKeyword = MatchToken(SyntaxKind.InputKeyword)
      Dim optionalSemiColonToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.SemicolonToken Then optionalSemiColonToken = MatchToken(SyntaxKind.SemicolonToken)
      Dim optionalPromptExpression As ExpressionSyntax = Nothing
      Dim semiColonOrCommaToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.StringToken Then
        optionalPromptExpression = ParseExpression()
        Dim kind = SyntaxKind.SemicolonToken
        If Current.Kind = SyntaxKind.CommaToken Then kind = SyntaxKind.CommaToken
        semiColonOrCommaToken = MatchToken(kind)
      End If

      Dim identifiersAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxToken)()

      Dim parseNextIdentifier = True
      While parseNextIdentifier AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
        identifiersAndSeparators.Add(identifier)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          identifiersAndSeparators.Add(comma)
        Else
          parseNextIdentifier = False
        End If

      End While

      Return New InputStatementSyntax(m_syntaxTree, inputKeyword, optionalSemiColonToken, optionalPromptExpression, semiColonOrCommaToken, identifiersAndSeparators.ToImmutable())

    End Function

    Private Function ParseKillStatement() As KillStatementSyntax

      ' KILL path

      Dim killKeyword = MatchToken(SyntaxKind.KillKeyword)
      Dim path = ParseExpression()
      Return New KillStatementSyntax(m_syntaxTree, killKeyword, path)

    End Function

    Private Function ParseLetStatement() As StatementSyntax

      ' LET *identifier* = *expression*

      Dim letKeywordToken = MatchToken(SyntaxKind.LetKeyword)
      Dim identifierToken = NextToken()
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()
      Return New LetStatementSyntax(m_syntaxTree, letKeywordToken, identifierToken, equalToken, expression)

    End Function

    Private Function ParseMkDirStatement() As MkDirStatementSyntax

      ' MKDIR path

      Dim mkDirKeyword = MatchToken(SyntaxKind.MkDirKeyword)
      Dim path = ParseExpression()
      Return New MkDirStatementSyntax(m_syntaxTree, mkDirKeyword, path)

    End Function

    Private Function ParseMidStatement() As MidStatementSyntax
      Dim midKeyword = MatchToken(SyntaxKind.MidKeyword)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
      Dim positionCommaToken = MatchToken(SyntaxKind.CommaToken)
      Dim position = ParseExpression()
      Dim lengthCommaToken As SyntaxToken = Nothing
      Dim length As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        lengthCommaToken = MatchToken(SyntaxKind.CommaToken)
        length = ParseExpression()
      End If
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()
      Return New MidStatementSyntax(m_syntaxTree, midKeyword, openParen, identifierToken, positionCommaToken, position, lengthCommaToken, length, closeParen, equalToken, expression)
    End Function

    Private Function ParseNameStatement() As NameStatementSyntax

      ' NAME path2 AS path2

      Dim nameKeyword = MatchToken(SyntaxKind.NameKeyword)
      Dim originalPath = ParseExpression()
      Dim asKeyword = MatchToken(SyntaxKind.AsKeyword)
      Dim destinationPath = ParseExpression()
      Return New NameStatementSyntax(m_syntaxTree, nameKeyword, originalPath, asKeyword, destinationPath)

    End Function

    Private Function ParseOptionStatement() As StatementSyntax

      ' OPTION BASE {0|1}

      Dim optionKeyword = MatchToken(SyntaxKind.OptionKeyword)
      Dim baseKeyword = MatchToken(SyntaxKind.BaseKeyword)
      If Not (Current.Kind = SyntaxKind.NumberToken AndAlso
              (Current.Text = "0" OrElse Current.Text = "1")) Then
        m_diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, SyntaxKind.NumberToken)
      End If
      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New OptionStatementSyntax(m_syntaxTree, optionKeyword, baseKeyword, numberToken)

    End Function

    Private Function ParseReadStatement() As ReadStatementSyntax

      ' READ variablelist

      Dim readKeyword = MatchToken(SyntaxKind.ReadKeyword)

      Dim identifiersAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxToken)()

      Dim parseNextIdentifier = True
      While parseNextIdentifier AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
        identifiersAndSeparators.Add(identifier)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          identifiersAndSeparators.Add(comma)
        Else
          parseNextIdentifier = False
        End If

      End While

      Return New ReadStatementSyntax(m_syntaxTree, readKeyword, identifiersAndSeparators.ToImmutable())

    End Function

    Private Function ParseRemStatement() As RemStatementSyntax

      Dim remKeyword = MatchToken(SyntaxKind.RemKeyword)
      Dim remLine = m_text.GetLineIndex(remKeyword.Span.Start)

      Dim comment As String = ""

      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        If currentLine <> remLine Then Exit While

        Dim token = NextToken()
        For Each entry In token.LeadingTrivia
          comment &= entry.Text
        Next
        comment &= token.Text
        For Each entry In token.TrailingTrivia
          comment &= entry.Text
        Next

      End While

      Return New RemStatementSyntax(m_syntaxTree, remKeyword, comment)

    End Function

    Private Function ParseRestoreStatement() As RestoreStatementSyntax

      ' RESTORE [line]

      Dim restoreKeyword = MatchToken(SyntaxKind.RestoreKeyword)

      Dim numberToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.NumberToken Then
        numberToken = MatchToken(SyntaxKind.NumberToken)
      End If

      Return New RestoreStatementSyntax(m_syntaxTree, restoreKeyword, numberToken)

    End Function

    Private Function ParseRmDirStatement() As RmDirStatementSyntax

      ' RMDIR path

      Dim rmDirKeyword = MatchToken(SyntaxKind.RmDirKeyword)
      Dim path = ParseExpression()
      Return New RmDirStatementSyntax(m_syntaxTree, rmDirKeyword, path)

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

      Dim lastPosition = Current.Position
      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        If currentLine <> printLine Then Exit While

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
        ElseIf Current.Kind = SyntaxKind.SemicolonToken Then
          Dim semiColonToken = MatchToken(SyntaxKind.SemicolonToken)
          'If lastToken <> SyntaxKind.SemicolonToken Then
          nodes.Add(semiColonToken)
          'End If
        Else
          Dim expression = ParseExpression()
          If expression IsNot Nothing Then
            'If lastToken = SyntaxKind.ExpressionStatement Then
            '  Dim semiColonToken = New SyntaxToken(m_syntaxTree, SyntaxKind.SemicolonToken, expression.Span.Start, ";", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
            '  nodes.Add(semiColonToken)
            'End If
            nodes.Add(expression)
          End If
        End If

        If Current.Position = lastPosition Then
          ' stuck?
          Exit While
        Else
          lastPosition = Current.Position
        End If

      End While

      Return New PrintStatementSyntax(m_syntaxTree, printKeyword, If(nodes IsNot Nothing, nodes.ToImmutable, Nothing))

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
      Dim statements = ParseBlockStatement(False)
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

    Private Function ParseReturnStatement(isTopLevel As Boolean) As StatementSyntax

      If isTopLevel Then

        ' RETURN [*label*]

        ' RETURN [*line number*]

        Dim returnKeyword = MatchToken(SyntaxKind.ReturnKeyword)
        Dim keywordLine = m_text.GetLineIndex(returnKeyword.Span.Start)
        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        Dim isEof = Current.Kind = SyntaxKind.EndOfFileToken
        Dim sameLine = Not isEof AndAlso keywordLine = currentLine

        Dim token As SyntaxToken = Nothing
        If sameLine Then
          Dim kind = SyntaxKind.NumberToken
          If Current.Kind = SyntaxKind.IdentifierToken Then
            kind = SyntaxKind.IdentifierToken
          End If
          token = MatchToken(kind)
        End If
        Return New ReturnGosubStatementSyntax(m_syntaxTree, returnKeyword, token)
      Else

        ' RETURN [*expression*]

        Dim returnKeyword = MatchToken(SyntaxKind.ReturnKeyword)
        Dim keywordLine = m_text.GetLineIndex(returnKeyword.Span.Start)
        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        Dim isEof = Current.Kind = SyntaxKind.EndOfFileToken
        Dim sameLine = Not isEof AndAlso keywordLine = currentLine
        Dim expression = If(sameLine, ParseExpression(), Nothing)
        Return New ReturnStatementSyntax(m_syntaxTree, returnKeyword, expression)

      End If

    End Function

    Private Function ParseLabelStatement() As LabelStatementSyntax

      ' DoSomething:

      Dim label = MatchToken(SyntaxKind.Label)
      Return New LabelStatementSyntax(m_syntaxTree, label)

    End Function

    Private Function ParseEndStatement() As EndStatementSyntax

      ' End

      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Return New EndStatementSyntax(m_syntaxTree, endKeyword)

    End Function

    Private Function ParseExitStatement() As ExitStatementSyntax

      ' Exit Def
      ' Exit Do
      ' Exit For
      ' Exit Function
      ' Exit Sub
      ' Exit While

      Dim exitKeyword = MatchToken(SyntaxKind.ExitKeyword)
      Dim kind As SyntaxKind = SyntaxKind.ForKeyword
      Select Case Current.Kind
        Case SyntaxKind.DefKeyword,
             SyntaxKind.DoKeyword,
             SyntaxKind.ForKeyword,
             SyntaxKind.FunctionKeyword,
             SyntaxKind.SubKeyword,
             SyntaxKind.WhileKeyword
          kind = Current.Kind
        Case Else
      End Select
      Dim scopeKeyword = MatchToken(kind)
      Return New ExitStatementSyntax(m_syntaxTree, exitKeyword, scopeKeyword)

    End Function

    Private Function ParseContinueStatement() As ContinueStatementSyntax

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

    Private Function ParseDoStatement(isTopLevel As Boolean) As StatementSyntax

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

      Dim body = ParseBlockStatement(isTopLevel)

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

    Private Function ParseForStatement(isTopLevel As Boolean) As StatementSyntax

      If Current.Kind = SyntaxKind.ForKeyword AndAlso Peek(1).Kind = SyntaxKind.EachKeyword Then
        Return ParseForEachStatement(isTopLevel)
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
      Dim statements = ParseBlockStatement(isTopLevel)
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

    Private Function IsPossibleExpression() As Boolean
      ' 1
      ' 1 + 1
      ' 1 + a
      ' a
      ' a + a
      ' a + 1
      ' (a)
      ' (1)
      ' int(value)
      Return Current.Kind = SyntaxKind.NumberToken OrElse
             Current.Kind = SyntaxKind.IdentifierToken OrElse
             Current.Kind = SyntaxKind.OpenParenToken OrElse
             Current.Kind.Is_Keyword
    End Function

    Private Function ParseForEachStatement(isTopLevel As Boolean) As StatementSyntax

      ' FOR EACH *value* IN *array*
      '   *statements*
      ' NEXT

      Dim forKeyword = MatchToken(SyntaxKind.ForKeyword)
      Dim eachKeyword = MatchToken(SyntaxKind.EachKeyword)
      Dim value = MatchToken(SyntaxKind.IdentifierToken)
      Dim inKeyword = MatchToken(SyntaxKind.InKeyword)
      Dim array = MatchToken(SyntaxKind.IdentifierToken)
      Dim statements = ParseBlockStatement(isTopLevel)
      Dim nextKeyword = MatchToken(SyntaxKind.NextKeyword)
      Return New ForEachStatementSyntax(m_syntaxTree, forKeyword,
                                        eachKeyword,
                                        value,
                                        inKeyword,
                                        array,
                                        statements,
                                        nextKeyword)
    End Function

    Private Function ParseStopStatement() As StatementSyntax

      ' STOP

      Dim stopKeyword = MatchToken(SyntaxKind.StopKeyword)
      Return New StopStatementSyntax(m_syntaxTree, stopKeyword)

    End Function

    Private Function ParseSystemStatement() As StatementSyntax

      ' SYSTEM

      Dim systemKeyword = MatchToken(SyntaxKind.SystemKeyword)
      Return New SystemStatementSyntax(m_syntaxTree, systemKeyword)

    End Function

    Private Function ParseWhileStatement(isTopLevel As Boolean) As StatementSyntax

      ' WHILE [*boolean_expression*]
      '   *statements*
      ' WEND

      ' or

      ' WHILE [*boolean_expression*]
      '   *statements*
      ' END WHILE

      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim expression = ParseExpression()
      Dim body = ParseBlockStatement(isTopLevel)
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

    Private Function ParseIfStatement(isTopLevel As Boolean) As StatementSyntax

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

      Dim statements = ParseBlockStatement(isTopLevel, Not multiLine)

      If multiLine Then
        'Dim ifStatement = New IfStatementSyntax(m_syntaxTree, ifKeyword, expression, thenKeyword, statements)
        'TODO: Need to handle ElseIf...
        'Dim elseIfStatements = ImmutableArray(Of ElseIfStatementSyntax).Empty
        Dim elseClause = ParseOptionalElseClauseSyntax(isTopLevel)
        Dim endIfKeyword = MatchToken(SyntaxKind.EndIfKeyword)
        Return New IfStatementSyntax(m_syntaxTree,
                                     ifKeyword,
                                     expression,
                                     thenKeyword,
                                     statements,
                                     elseClause,
                                     endIfKeyword)
      Else
        Dim elseClause = ParseOptionalSingleLineElseClause(isTopLevel)
        Return New SingleLineIfStatementSyntax(m_syntaxTree,
                                               ifKeyword,
                                               expression,
                                               thenKeyword,
                                               statements,
                                               elseClause)
      End If

    End Function

    Private Function ParseOptionalSingleLineElseClause(isTopLevel As Boolean) As SingleLineElseClauseSyntax

      ' ... ELSE *statements*

      If Current.Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseBlockStatement(isTopLevel)
      Return New SingleLineElseClauseSyntax(m_syntaxTree, elseKeyword, statements)

    End Function

    Private Function ParseOptionalElseClauseSyntax(isTopLevel As Boolean) As ElseClauseSyntax

      ' ...
      ' ELSE
      '   *statements*
      ' ...

      If Current.Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseBlockStatement(isTopLevel)
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

    Private Function ParseBlockStatement(isTopLevel As Boolean, Optional singleLine As Boolean = False) As BlockStatementSyntax
      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
      Dim openBraceToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.OpenBraceToken Then
        openBraceToken = MatchToken(SyntaxKind.OpenBraceToken)
      End If
      Dim startToken As SyntaxToken '= Current()

      Dim beginLine = m_text.GetLineIndex(Current.Span.Start)

      While Current.Kind <> SyntaxKind.EndOfFileToken AndAlso Not IsEndOfBlock()
        'Current.Kind <> SyntaxKind.CloseBraceToken

        If singleLine Then
          Dim peekLine = m_text.GetLineIndex(Current.Span.Start)
          If peekLine > beginLine Then
            Exit While
          End If
        End If

        startToken = Current()
        Dim statement = ParseStatement(isTopLevel)
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
Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Lowering
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax
Imports Bsharp.CodeAnalysis.Text

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private Const GOTO_LABEL_PREFIX As String = "$LABEL"

    Private m_scope As BoundScope
    Private ReadOnly m_isScript As Boolean
    Private ReadOnly m_function As FunctionSymbol
    Private ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag
    Private ReadOnly m_loopStack As New Stack(Of (ExitLabel As BoundLabel, ContinueLabel As BoundLabel))
    Private m_labelCounter As Integer

    Public Sub New(isScript As Boolean, parent As BoundScope, [function] As FunctionSymbol)
      m_scope = New BoundScope(parent)
      m_isScript = isScript
      m_function = [function]
      If [function] IsNot Nothing Then
        For Each p In [function].Parameters
          m_scope.TryDeclareVariable(p)
        Next
      End If
    End Sub

    Public Shared Function BindGlobalScope(isScript As Boolean, previous As BoundGlobalScope, syntaxTrees As ImmutableArray(Of SyntaxTree)) As BoundGlobalScope

      Dim parentScope = CreateParentScope(previous)
      Dim binder = New Binder(isScript, parentScope, Nothing)

      binder.Diagnostics.AddRange(syntaxTrees.SelectMany(Function(st) st.Diagnostics))
      If binder.Diagnostics.Any Then
        Return New BoundGlobalScope(previous, binder.Diagnostics.ToImmutableArray, Nothing, Nothing, ImmutableArray(Of FunctionSymbol).Empty, ImmutableArray(Of VariableSymbol).Empty, ImmutableArray(Of BoundStatement).Empty)
      End If

      Dim functionDeclarations = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of FunctionDeclarationSyntax)

      For Each func In functionDeclarations
        binder.BindFunctionDeclaration(func)
      Next

      Dim globalStatements = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of GlobalStatementSyntax)

      ' Determine if any GOTO or GOSUB statements target a numeric value (Line Number).
      Dim targetLineNumbers As New List(Of Integer)
      For Each statement In globalStatements

        Dim target As New Integer?

        If TypeOf statement.Statement Is GotoStatementSyntax Then
          Dim g = CType(statement.Statement, GotoStatementSyntax)
          If IsNumeric(g.TargetToken.Text) Then
            target = CInt(g.TargetToken.Text)
          Else
            Continue For
          End If
        ElseIf TypeOf statement.Statement Is SingleLineIfStatementSyntax Then
          ' Any GOTO statement(s)?
        Else
          Continue For
        End If

        If target IsNot Nothing Then
          Dim value = CInt(target)
          If Not targetLineNumbers.Contains(value) Then
            targetLineNumbers.Add(value)
          End If
        End If

      Next

      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      For Each globalStatement In globalStatements

        Dim lineNumbers = IterateLineNumbers(globalStatement.Statement)

        For Each token In lineNumbers
          For Each trivia In token.LeadingTrivia
            If trivia.Kind = SyntaxKind.LineNumberTrivia Then
              Dim value = CInt(trivia.Text)
              Dim label = New SyntaxToken(globalStatement.SyntaxTree, SyntaxKind.LabelStatement, token.Position, $"{GOTO_LABEL_PREFIX}{value}:", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
              Dim stmt = New LabelStatementSyntax(globalStatement.SyntaxTree, label)
              statements.Add(binder.BindGlobalStatement(stmt))
            End If
          Next
        Next

        ''TODO: Need to figure out a way to significantly improve the following code.
        'For Each child In globalStatement.Statement.GetChildren
        '  If child.Kind.Is_Keyword Then
        '    Dim token = TryCast(child, SyntaxToken)
        '    If token IsNot Nothing Then
        '      For Each trivia In token.LeadingTrivia
        '        If trivia.Kind = SyntaxKind.LineNumberTrivia Then
        '          Dim value = CInt(trivia.Text)
        '          If True OrElse targetLineNumbers.Contains(value) Then
        '            ' matching target
        '            'TODO: Need to transform the LineNumberTrivia into a numbered Label.
        '            Dim label = New SyntaxToken(globalStatement.SyntaxTree, SyntaxKind.LabelStatement, token.Position, $"{GOTO_LABEL_PREFIX}{value}:", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
        '            Dim stmt = New LabelStatementSyntax(globalStatement.SyntaxTree, label)
        '            statements.Add(binder.BindGlobalStatement(stmt))
        '          End If
        '        End If
        '      Next
        '    End If
        '  End If
        'Next

        Dim statement = binder.BindGlobalStatement(globalStatement.Statement)
        statements.Add(statement)

      Next

      ' Check global statements.

      Dim firstGlobalStatementPerSyntaxTree = syntaxTrees.Select(Function(st) st.Root.Members.OfType(Of GlobalStatementSyntax).FirstOrDefault).
                                                          Where(Function(g) g IsNot Nothing).
                                                          ToArray

      If firstGlobalStatementPerSyntaxTree.Length > 1 Then
        For Each globalStatement In firstGlobalStatementPerSyntaxTree
          binder.Diagnostics.ReportOnlyOneFileCanHaveGlobalStatements(globalStatement.Location)
        Next
      End If

      ' Check for main/script with global statements.

      Dim functions = binder.m_scope.GetDeclaredFunctions

      Dim mainFunction As FunctionSymbol
      Dim scriptFunction As FunctionSymbol

      If isScript Then

        mainFunction = Nothing

        If globalStatements.Any Then
          scriptFunction = New FunctionSymbol("$eval", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.Any, Nothing)
        Else
          scriptFunction = Nothing
        End If

      Else

        mainFunction = functions.FirstOrDefault(Function(f) f.Name = "main")
        scriptFunction = Nothing

        If mainFunction IsNot Nothing Then
          If mainFunction.Type IsNot TypeSymbol.Nothing OrElse
             mainFunction.Parameters.Any Then
            binder.Diagnostics.ReportMainMustHaveCorrectSignature(mainFunction.Declaration.Identifier.Location)
          End If
        End If

        If globalStatements.Any Then
          If mainFunction IsNot Nothing Then
            binder.Diagnostics.ReportCannotMixMainAndGlobalStatements(mainFunction.Declaration.Identifier.Location)
            For Each globalStatement In firstGlobalStatementPerSyntaxTree
              binder.Diagnostics.ReportCannotMixMainAndGlobalStatements(globalStatement.Location)
            Next
          Else
            mainFunction = New FunctionSymbol("main", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.Nothing)
          End If
        End If

      End If

      Dim diagnostics = binder.Diagnostics.ToImmutableArray

      Dim variables = binder.m_scope.GetDeclaredVariables

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      Return New BoundGlobalScope(previous, diagnostics, mainFunction, scriptFunction, functions, variables, statements.ToImmutable)

    End Function

    Private Shared Function IterateLineNumbers(statement As SyntaxNode) As List(Of SyntaxToken)

      Dim results As New List(Of SyntaxToken)

      For Each child In statement.GetChildren
        'If child.Kind.Is_Keyword Then
        Dim token = TryCast(child, SyntaxToken)
        If token IsNot Nothing Then
          For Each trivia In token.LeadingTrivia
            If trivia.Kind = SyntaxKind.LineNumberTrivia Then
              'Dim value = CInt(trivia.Text)
              'If True Then 'OrElse targetLineNumbers.Contains(value) Then
              'TODO: Need to transform the LineNumberTrivia into a numbered Label.
              'Dim label = New SyntaxToken(globalStatement.SyntaxTree, SyntaxKind.LabelStatement, token.Position, $"{GOTO_LABEL_PREFIX}{value}:", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
              'Dim stmt = New LabelStatementSyntax(globalStatement.SyntaxTree, label)
              'statements.Add(Binder.BindGlobalStatement(stmt))
              results.Add(token)
              'End If
            End If
          Next
        End If
        'End If
        Dim r = IterateLineNumbers(child)
        If r.Count > 0 Then
          For Each entry In r
            results.Add(entry)
          Next
        End If
        'If TypeOf child Is StatementSyntax Then
        '  Dim r = IterateLineNumbers(CType(child, StatementSyntax))
        '  If r.Count > 0 Then
        '    For Each entry In r
        '      results.Add(entry)
        '    Next
        '  End If
        'ElseIf TypeOf child Is ExpressionSyntax Then
        '  Dim r = IterateLineNumbers(CType(child, StatementSyntax))
        '  If r.Count > 0 Then
        '    For Each entry In r
        '      results.Add(entry)
        '    Next
        '  End If
        'End If
      Next

      Return results

    End Function

    Public Shared Function BindProgram(isScript As Boolean, previous As BoundProgram, globalScope As BoundGlobalScope) As BoundProgram

      Dim parentScope = CreateParentScope(globalScope)

      If globalScope.Diagnostics.Any Then
        Return New BoundProgram(previous, globalScope.Diagnostics, Nothing, Nothing, ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement).Empty)
      End If

      Dim functionBodies = ImmutableDictionary.CreateBuilder(Of FunctionSymbol, BoundBlockStatement)()
      Dim diagnostics = ImmutableArray.CreateBuilder(Of Diagnostic)()

      For Each func In globalScope.Functions
        Dim binder = New Binder(isScript, parentScope, func)
        Dim body = binder.BindStatement(func.Declaration.Statements)
        Dim loweredBody = Lowerer.Lower(func, body)
        If func.Type IsNot TypeSymbol.Nothing AndAlso Not ControlFlowGraph.AllPathsReturn(loweredBody) Then
          binder.Diagnostics.ReportAllPathsMustReturn(func.Declaration.Identifier.Location)
        End If
        functionBodies.Add(func, loweredBody)
        diagnostics.AddRange(binder.Diagnostics)
      Next

      If globalScope.MainFunction IsNot Nothing AndAlso globalScope.Statements.Any Then
        Dim body = Lowerer.Lower(globalScope.MainFunction, New BoundBlockStatement(globalScope.Statements))
        functionBodies.Add(globalScope.MainFunction, body)
      ElseIf globalScope.ScriptFunction IsNot Nothing Then
        Dim statements = globalScope.Statements
        Dim es = TryCast(statements(0), BoundExpressionStatement)
        Dim needsReturn = statements.Length = 1 AndAlso
                          TypeOf es Is BoundExpressionStatement AndAlso
                          es.Expression.Type IsNot TypeSymbol.Nothing
        If needsReturn Then
          statements = statements.SetItem(0, New BoundReturnStatement(es.Expression))
        ElseIf statements.Any AndAlso
               statements.Last.Kind <> BoundNodeKind.ReturnStatement Then
          Dim nullValue = New BoundLiteralExpression("")
          statements = statements.Add(New BoundReturnStatement(nullValue))
        End If
        Dim body = Lowerer.Lower(globalScope.ScriptFunction, New BoundBlockStatement(statements))
        functionBodies.Add(globalScope.ScriptFunction, body)
      End If

      Return New BoundProgram(previous, diagnostics.ToImmutable, globalScope.MainFunction, globalScope.ScriptFunction, functionBodies.ToImmutable)

    End Function

    Private Sub BindFunctionDeclaration(syntax As FunctionDeclarationSyntax)

      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)()

      Dim seenParameterNames As New HashSet(Of String)

      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Text
        Dim parameterType = BindAsClause(parameterSyntax.AsClause)
        If Not seenParameterNames.Add(parameterName) Then
          Diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Location, parameterName)
        Else
          Dim parameter As New ParameterSymbol(parameterName, parameterType, parameters.Count)
          parameters.Add(parameter)
        End If
      Next

      Dim type = If(BindAsClause(syntax.AsClause), TypeSymbol.Nothing)

      Dim func As New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable(), type, syntax)
      'If func.Declaration.Identifier.Text IsNot Nothing AndAlso
      If syntax.Identifier.Text IsNot Nothing AndAlso
         Not m_scope.TryDeclareFunction(func) Then
        Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, func.Name)
      End If

    End Sub

    Private Shared Function CreateParentScope(previous As BoundGlobalScope) As BoundScope
      Dim stack = New Stack(Of BoundGlobalScope)
      While previous IsNot Nothing
        stack.Push(previous)
        previous = previous.Previous
      End While
      Dim parent = CreateRootScope()
      While stack.Count > 0
        previous = stack.Pop
        Dim scope = New BoundScope(parent)
        For Each f In previous.Functions
          scope.TryDeclareFunction(f)
        Next
        For Each v In previous.Variables
          scope.TryDeclareVariable(v)
        Next
        parent = scope
      End While
      Return parent
    End Function

    Private Shared Function CreateRootScope() As BoundScope
      Dim result = New BoundScope(Nothing)
      For Each f In BuiltinFunctions.GetAll
        'TODO: Handle overloading/optional parameters.
        result.TryDeclareFunction(f)
      Next
      Return result
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Return BindConversion(syntax, targetType)
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, Optional canBeVoid As Boolean = False) As BoundExpression
      Dim result = BindExpressionInternal(syntax)
      If Not canBeVoid AndAlso result.Type Is TypeSymbol.Nothing Then
        Diagnostics.ReportExpressionMustHaveValue(syntax.Location)
        Return New BoundErrorExpression
      End If
      Return result
    End Function

    Private Function BindExpressionInternal(syntax As ExpressionSyntax) As BoundExpression
      Select Case syntax.Kind
        Case SyntaxKind.ParenExpression : Return BindParenExpression(CType(syntax, ParenExpressionSyntax))
        Case SyntaxKind.LiteralExpression : Return BindLiteralExpression(CType(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.NameExpression : Return BindNameExpression(CType(syntax, NameExpressionSyntax))
        Case SyntaxKind.AssignmentExpression : Return BindAssignmentExpression(CType(syntax, AssignmentExpressionSyntax))
        Case SyntaxKind.UnaryExpression : Return BindUnaryExpression(CType(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression : Return BindBinaryExpression(CType(syntax, BinaryExpressionSyntax))
        Case SyntaxKind.CallExpression : Return BindCallExpression(CType(syntax, CallExpressionSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
    End Function

    Private Shared Function BindErrorStatement() As BoundStatement
      Return New BoundExpressionStatement(New BoundErrorExpression)
    End Function

    Private Function BindGlobalStatement(syntax As StatementSyntax) As BoundStatement
      Return BindStatement(syntax, True)
    End Function

    Private Function BindStatement(syntax As StatementSyntax, Optional isGlobal As Boolean = False) As BoundStatement
      Dim result = BindStatementInternal(syntax)
      If Not m_isScript Or Not isGlobal Then
        If TypeOf result Is BoundExpressionStatement Then
          Dim es = CType(result, BoundExpressionStatement)
          Dim isAllowedExpression = es.Expression.Kind = BoundNodeKind.ErrorExpression Or
                                    es.Expression.Kind = BoundNodeKind.AssignmentExpression Or
                                    es.Expression.Kind = BoundNodeKind.CallExpression
          If Not isAllowedExpression Then
            Diagnostics.ReportInvalidExpressionStatement(syntax.Location)
          End If
        End If
      End If
      Return result
    End Function

    Private Function BindStatementInternal(syntax As StatementSyntax, Optional isGlobal As Boolean = False) As BoundStatement
      If isGlobal Then
      End If
      Select Case syntax.Kind
        Case SyntaxKind.BlockStatement : Return BindBlockStatement(CType(syntax, BlockStatementSyntax))
        Case SyntaxKind.ChDirStatement : Return BindChDirStatement(CType(syntax, ChDirStatementSyntax))
        Case SyntaxKind.ClearStatement : Return BindClearStatement(CType(syntax, ClearStatementSyntax))
        Case SyntaxKind.ClsStatement : Return BindClsStatement(CType(syntax, ClsStatementSyntax))
        Case SyntaxKind.ContinueStatement : Return BindContinueStatement(CType(syntax, ContinueStatementSyntax))
        Case SyntaxKind.DoUntilStatement : Return BindDoUntilStatement(CType(syntax, DoUntilStatementSyntax))
        Case SyntaxKind.DoWhileStatement : Return BindDoWhileStatement(CType(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.EndStatement : Return BindEndStatement(CType(syntax, EndStatementSyntax))
        Case SyntaxKind.ExitStatement : Return BindExitStatement(CType(syntax, ExitStatementSyntax))
        Case SyntaxKind.ExpressionStatement : Return BindExpressionStatement(CType(syntax, ExpressionStatementSyntax))
        Case SyntaxKind.ForStatement : Return BindForStatement(CType(syntax, ForStatementSyntax))
        Case SyntaxKind.GosubStatement : Return BindGosubStatement(CType(syntax, GosubStatementSyntax))
        Case SyntaxKind.GotoStatement : Return BindGotoStatement(CType(syntax, GotoStatementSyntax))
        Case SyntaxKind.IfStatement : Return BindIfStatement(CType(syntax, IfStatementSyntax))
        Case SyntaxKind.InputStatement : Return BindInputStatement(CType(syntax, InputStatementSyntax))
        Case SyntaxKind.KillStatement : Return BindKillStatement(CType(syntax, KillStatementSyntax))
        Case SyntaxKind.LabelStatement : Return BindLabelStatement(CType(syntax, LabelStatementSyntax))
        Case SyntaxKind.LetStatement : Return BindLetStatement(CType(syntax, LetStatementSyntax))
        Case SyntaxKind.MidStatement : Return BindMidStatement(CType(syntax, MidStatementSyntax))
        Case SyntaxKind.MkDirStatement : Return BindMkDirStatement(CType(syntax, MkDirStatementSyntax))
        Case SyntaxKind.NameStatement : Return BindNameStatement(CType(syntax, NameStatementSyntax))
        Case SyntaxKind.OptionStatement : Return BindOptionStatement(CType(syntax, OptionStatementSyntax))
        Case SyntaxKind.PrintStatement : Return BindPrintStatement(CType(syntax, PrintStatementSyntax))
        Case SyntaxKind.RemStatement : Return BindRemStatement(CType(syntax, RemStatementSyntax))
        Case SyntaxKind.ReturnGosubStatement : Return BindReturnGosubStatement(CType(syntax, ReturnGosubStatementSyntax))
        Case SyntaxKind.ReturnStatement : Return BindReturnStatement(CType(syntax, ReturnStatementSyntax))
        Case SyntaxKind.RmDirStatement : Return BindRmDirStatement(CType(syntax, RmDirStatementSyntax))
        Case SyntaxKind.SingleLineIfStatement : Return BindSingleLineIfStatement(CType(syntax, SingleLineIfStatementSyntax))
        Case SyntaxKind.StopStatement : Return BindStopStatement(CType(syntax, StopStatementSyntax))
        Case SyntaxKind.SystemStatement : Return BindSystemStatement(CType(syntax, SystemStatementSyntax))
        Case SyntaxKind.VariableDeclarationStatement : Return BindVariableDeclaration(CType(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.WhileStatement : Return BindWhileStatement(CType(syntax, WhileStatementSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
    End Function

    Private Function BindAsClause(syntax As AsClauseSyntax) As TypeSymbol
      If syntax Is Nothing Then Return Nothing
      Dim type = LookupType(syntax.Identifier.Text)
      If type Is Nothing Then
        Diagnostics.ReportUndefinedType(syntax.Identifier.Location, syntax.Identifier.Text)
        Return Nothing
      End If
      Return type
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = BindExpression(syntax.Expression)

      Dim variable = DetermineVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        ' Variable has not been declared, let's go ahead and do so.
        Dim type = TypeSymbol.String
        If Not syntax.IdentifierToken.Text.EndsWith("$") Then
          type = TypeSymbol.Double
        End If
        variable = BindVariableDeclaration(syntax.IdentifierToken, False, type) ' boundExpression.Type
      End If
      'Dim variable = BindVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        Return boundExpression
      End If

      If variable.IsReadOnly Then
        Diagnostics.ReportCannotAssign(syntax.EqualToken.Location, name)
      End If

      Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type)

      Return New BoundAssignmentExpression(variable, convertedExpression)

    End Function

    Private Function BindBinaryExpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = BindExpression(syntax.Left)
      Dim boundRight = BindExpression(syntax.Right)
      If boundLeft.Type Is TypeSymbol.Error OrElse boundRight.Type Is TypeSymbol.Error Then Return New BoundErrorExpression
      Dim boundOperator = BoundBinaryOperator.Bind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperator Is Nothing Then
        Diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Location, syntax.OperatorToken.Text, boundLeft.Type, boundRight.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperator, boundRight)
    End Function

    Private Function BindBlockStatement(syntax As BlockStatementSyntax) As BoundStatement
      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      m_scope = New BoundScope(m_scope)
      For Each statementSyntax In syntax.Statements
        Dim statement = BindStatement(statementSyntax)
        statements.Add(statement)
      Next
      m_scope = m_scope.Parent
      Return New BoundBlockStatement(statements.ToImmutable)
    End Function

    Private Function BindCallExpression(syntax As CallExpressionSyntax) As BoundExpression

      Dim t = LookupType(syntax.Identifier.Text)
      If syntax.Arguments.Count = 1 AndAlso TypeOf t Is TypeSymbol Then
        Return BindConversion(syntax.Arguments(0), t, True)
      End If

      Dim boundArguments = ImmutableArray.CreateBuilder(Of BoundExpression)()

      Dim parameters = New List(Of TypeSymbol)
      For Each argument In syntax.Arguments
        Dim boundArgument = BindExpression(argument)
        boundArguments.Add(boundArgument)
        parameters.Add(boundArgument.Type)
      Next

      'Dim symbol = m_scope.TryLookupSymbol(syntax.Identifier.Text)
      Dim symbol = m_scope.TryLookupFunction(syntax.Identifier.Text, parameters)
      If symbol Is Nothing Then
        Diagnostics.ReportUndefinedFunction(syntax.Identifier.Location, syntax.Identifier.Text)
        Return New BoundErrorExpression
      End If

      Dim func = TryCast(symbol, FunctionSymbol)
      If func Is Nothing Then
        Diagnostics.ReportNotAFunction(syntax.Identifier.Location, syntax.Identifier.Text)
        Return New BoundErrorExpression
      End If

      If syntax.Arguments.Count <> func.Parameters.Length Then
        Dim span As TextSpan
        If syntax.Arguments.Count > func.Parameters.Length Then
          Dim firstExceedingNode As SyntaxNode
          If func.Parameters.Length > 0 Then
            firstExceedingNode = syntax.Arguments.GetSeparator(func.Parameters.Length - 1)
          Else
            firstExceedingNode = syntax.Arguments(0)
          End If
          Dim lastExceedingArgument = syntax.Arguments(syntax.Arguments.Count - 1)
          span = TextSpan.FromBounds(firstExceedingNode.Span.Start, lastExceedingArgument.Span.[End])
        Else
          span = syntax.CloseParenToken.Span
        End If
        Dim location = New TextLocation(syntax.SyntaxTree.Text, span)
        Diagnostics.ReportWrongArgumentCount(location, func.Name, func.Parameters.Length, syntax.Arguments.Count)
        Return New BoundErrorExpression
      End If

      For i = 0 To syntax.Arguments.Count - 1

        Dim argumentLocation = syntax.Arguments(i).Location
        Dim argument = boundArguments(i)
        Dim parameter = func.Parameters(i)

        boundArguments(i) = BindConversion(argumentLocation, argument, parameter.Type)

      Next

      Return New BoundCallExpression(func, boundArguments.ToImmutable())

    End Function

    Private Function BindChDirStatement(syntax As ChDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundChDirStatement(path)
    End Function

    Private Function BindClearStatement(syntax As ClearStatementSyntax) As BoundStatement
      Dim maxBytesExpression = If(syntax.MaxBytesExpression IsNot Nothing, BindExpression(syntax.MaxBytesExpression), Nothing)
      Dim stackSpaceExpression = If(syntax.StackSpaceExpression IsNot Nothing, BindExpression(syntax.StackSpaceExpression), Nothing)
      Return New BoundClearStatement(maxBytesExpression, stackSpaceExpression)
    End Function

    Private Function BindClsStatement(syntax As ClsStatementSyntax) As BoundStatement
      Dim expression = If(syntax.Expression IsNot Nothing, BindExpression(syntax.Expression), Nothing)
      Return New BoundClsStatement(expression)
    End Function

    Private Function BindContinueStatement(syntax As ContinueStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ContinueKeyword.Location, syntax.ContinueKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim continueLabel = m_loopStack.Peek().ContinueLabel
      Return New BoundGotoStatement(continueLabel)

    End Function

    Private Function BindConversion(syntax As ExpressionSyntax, [type] As TypeSymbol, Optional allowExplicit As Boolean = False) As BoundExpression
      Dim expression = BindExpression(syntax)
      Return BindConversion(syntax.Location, expression, type, allowExplicit)
    End Function

    Private Function BindConversion(diagnosticLocation As TextLocation,
                                    expression As BoundExpression,
                                    type As TypeSymbol,
                                    Optional allowExplicit As Boolean = False) As BoundExpression
      Dim c = Conversion.Classify(expression.Type, [type])
      If Not c.Exists Then
        If expression.Type IsNot TypeSymbol.Error AndAlso [type] IsNot TypeSymbol.Error Then
          Diagnostics.ReportCannotConvert(diagnosticLocation, expression.Type, [type])
        End If
        Return New BoundErrorExpression
      End If
      If Not allowExplicit AndAlso c.IsExplicit Then
        Diagnostics.ReportCannotConvertImplicitly(diagnosticLocation, expression.Type, [type])
      End If

      If c.IsIdentity Then Return expression
      Return New BoundConversionExpression([type], expression)
    End Function

    Private Function BindDoUntilStatement(syntax As DoUntilStatementSyntax) As BoundStatement
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Dim expression = BindExpression(syntax.UntilClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.UntilClause.AtBeginning
      Return New BoundDoUntilStatement(statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Function BindDoWhileStatement(syntax As DoWhileStatementSyntax) As BoundStatement
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Dim expression = BindExpression(syntax.WhileClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.WhileClause.AtBeginning
      Return New BoundDoWhileStatement(statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Shared Function BindEndStatement(syntax As EndStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundEndStatement()
    End Function

    Private Function BindExitStatement(syntax As ExitStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ExitKeyword.Location, syntax.ExitKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim exitLabel = m_loopStack.Peek().ExitLabel
      Return New BoundGotoStatement(exitLabel)

    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, canBeVoid:=True)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.StartValue, TypeSymbol.Long)
      Dim upperBound = BindExpression(syntax.EndValue, TypeSymbol.Long)
      Dim stepper = If(syntax.Increment Is Nothing, Nothing, BindExpression(syntax.Increment, TypeSymbol.Long))

      m_scope = New BoundScope(m_scope)

      Dim variable = BindVariableDeclaration(syntax.Identifier, True, TypeSymbol.Long)
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim body = BindLoopBody(syntax.Statements, exitLabel, continueLabel)

      m_scope = m_scope.Parent

      Return New BoundForStatement(variable, lowerBound, upperBound, stepper, body, exitLabel, continueLabel)

    End Function

    Private Shared Function BindGosubStatement(syntax As GosubStatementSyntax) As BoundStatement
      Dim value = syntax.IdentifierToken.Text
      If IsNumeric(value) Then
        value = $"GosubLabel{value}"
      End If
      Dim label = New BoundLabel(value)
      Return New BoundGosubStatement(label)
    End Function

    Private Shared Function BindGotoStatement(syntax As GotoStatementSyntax) As BoundStatement
      Dim value = syntax.TargetToken.Text
      If IsNumeric(value) Then
        value = $"{GOTO_LABEL_PREFIX}{value}"
      End If
      Dim label = New BoundLabel(value)
      Return New BoundGotoStatement(label)
    End Function

    Private Function BindIfStatement(syntax As IfStatementSyntax) As BoundStatement

      'Dim ifStatement = syntax.IfStatement
      'Dim elseIfStatements = syntax.ElseIfStatements
      Dim elseStatement = syntax.ElseClause

      'TODO: Need to handle ElseIf...

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        ElseIf syntax.ElseClause IsNot Nothing Then
          Diagnostics.ReportUnreachableCode(syntax.ElseClause.Statements)
        End If
      End If

      Dim thenStatement = BindStatement(syntax.Statements)
      Dim elseClause = If(elseStatement IsNot Nothing, BindStatement(elseStatement.Statements), Nothing)
      Return New BoundIfStatement(condition, thenStatement, elseClause)

    End Function

    Private Function BindInputStatement(syntax As InputStatementSyntax) As BoundStatement

      Dim suppressCr = syntax.OptionalSemiColonToken IsNot Nothing
      Dim suppressQuestionMark = If(syntax.SemiColonOrCommaToken?.Kind = SyntaxKind.CommaToken, False)
      Dim prompt As BoundExpression = Nothing
      If syntax.OptionalPromptExpression IsNot Nothing Then
        prompt = BindExpression(syntax.OptionalPromptExpression, TypeSymbol.String)
      End If
      Dim variables As New List(Of VariableSymbol)
      For Each token In syntax.Tokens
        If token.Kind <> SyntaxKind.CommaToken Then

          'variables.Add(DetermineVariableReference(token))

          Dim variable = DetermineVariableReference(token)
          If variable Is Nothing Then
            If OPTION_EXPLICIT Then
              ' Variable appears to not have been already declared, 
              ' run through the normal process in order to generate
              ' the appropriate error(s).
              variable = BindVariableReference(token)
            Else
              ' Variable has not been declared, let's go ahead and do so.
              Dim type = TypeSymbol.String
              If Not token.Text.EndsWith("$") Then
                type = TypeSymbol.Double
              End If
              variable = BindVariableDeclaration(token, False, type)
            End If
          End If
          If variable Is Nothing Then
            Return Nothing
          End If

          variables.Add(variable)

        End If
      Next
      Return New BoundInputStatement(suppressCr, suppressQuestionMark, prompt, variables.ToImmutableArray)
    End Function

    Private Function BindKillStatement(syntax As KillStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundKillStatement(path)
    End Function

    Private Shared Function BindLabelStatement(syntax As LabelStatementSyntax) As BoundStatement
      Dim label = syntax.Label
      Dim boundLabel = New BoundLabel(label.Text.Substring(0, label.Text.Length - 1))
      Return New BoundLabelStatement(boundLabel)
    End Function

    Private Function BindLetStatement(syntax As LetStatementSyntax) As BoundStatement

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = BindExpression(syntax.Expression)

      Dim variable = DetermineVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        If OPTION_EXPLICIT Then
          ' Variable appears to not have been already declared, 
          ' run through the normal process in order to generate
          ' the appropriate error(s).
          variable = BindVariableReference(syntax.IdentifierToken)
        Else
          ' Variable has not been declared, let's go ahead and do so.
          Dim type = TypeSymbol.String
          If Not syntax.IdentifierToken.Text.EndsWith("$") Then
            type = TypeSymbol.Double
          End If
          variable = BindVariableDeclaration(syntax.IdentifierToken, False, type) ' boundExpression.Type
        End If
      End If
      If variable Is Nothing Then
        Return Nothing
      End If

      If variable.IsReadOnly Then
        Diagnostics.ReportCannotAssign(syntax.EqualToken.Location, name)
      End If

      Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type)

      Return New BoundLetStatement(variable, convertedExpression)

    End Function

    Private Shared Function BindLiteralExpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      Return New BoundLiteralExpression(value)
    End Function

    Private Function BindLoopBody(statements As StatementSyntax, ByRef exitLabel As BoundLabel, ByRef continueLabel As BoundLabel) As BoundStatement

      m_labelCounter += 1
      exitLabel = New BoundLabel($"exit{m_labelCounter}")
      continueLabel = New BoundLabel($"continue{m_labelCounter}")

      m_loopStack.Push((exitLabel, continueLabel))
      Dim boundBody = BindStatement(statements)
      m_loopStack.Pop()

      Return boundBody

    End Function

    Private Function BindMidStatement(syntax As MidStatementSyntax) As BoundStatement
      Dim variable = DetermineVariableReference(syntax.IdentifierToken)
      Dim positionExpression = If(syntax.PositionExpression Is Nothing, Nothing, BindExpression(syntax.PositionExpression))
      Dim lengthExpression = If(syntax.LengthExpression Is Nothing, Nothing, BindExpression(syntax.LengthExpression))
      Dim expression = If(syntax.Expression Is Nothing, Nothing, BindExpression(syntax.Expression))
      Return New BoundMidStatement(variable, positionExpression, lengthExpression, expression)
    End Function

    Private Function BindMkDirStatement(syntax As MkDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundMkDirStatement(path)
    End Function

    Private Function BindNameStatement(syntax As NameStatementSyntax) As BoundStatement
      Dim originalPath = BindExpression(syntax.OriginalPath)
      Dim destinationPath = BindExpression(syntax.DestinationPath)
      Return New BoundNameStatement(originalPath, destinationPath)
    End Function

    Private Function BindNameExpression(syntax As NameExpressionSyntax) As BoundExpression
      Dim name = syntax.IdentifierToken.Text
      If syntax.IdentifierToken.IsMissing Then
        ' This means the token was inserted by the parser. We already
        ' reported error so we can just return an error expression.
        Return New BoundErrorExpression
      End If
      Dim variable = BindVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        Return New BoundErrorExpression
      End If
      Return New BoundVariableExpression(variable)
    End Function

    Private Shared Function BindOptionStatement(syntax As OptionStatementSyntax) As BoundStatement
      Dim numberToken = syntax.NumberToken
      Return New BoundOptionStatement(CInt(numberToken.Text))
    End Function

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return BindExpression(syntax.Expression)
    End Function

    Private Function BindPrintStatement(syntax As PrintStatementSyntax) As BoundStatement

      Dim nodes = New List(Of BoundNode)

      For Each entry In syntax.Nodes
        If entry.Kind = SyntaxKind.SemicolonToken Then
          nodes.Add(New BoundSymbol(";"c))
        ElseIf entry.Kind = SyntaxKind.CommaToken Then
          nodes.Add(New BoundSymbol(","c))
        ElseIf entry.Kind = SyntaxKind.SpcFunction Then
          Dim spc = CType(entry, SpcFunctionSyntax)
          Dim expr = BindExpression(spc.Expression, TypeSymbol.Long)
          nodes.Add(New BoundSpcFunction(expr))
        ElseIf entry.Kind = SyntaxKind.TabFunction Then
          Dim tab = CType(entry, TabFunctionSyntax)
          Dim expr = BindExpression(tab.Expression, TypeSymbol.Long)
          nodes.Add(New BoundTabFunction(expr))
        Else
          nodes.Add(BindExpression(CType(entry, ExpressionSyntax), TypeSymbol.Any))
        End If
      Next

      Return New BoundPrintStatement(nodes.ToImmutableArray)

    End Function

    Private Shared Function BindRemStatement(syntax As RemStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundRemStatement()
    End Function

    Private Shared Function BindReturnGosubStatement(syntax As ReturnGosubStatementSyntax) As BoundStatement
      Dim value = syntax.TargetToken?.Text
      If value IsNot Nothing AndAlso IsNumeric(value) Then
        value = $"{GOTO_LABEL_PREFIX}{value}"
      End If
      Dim label = If(value IsNot Nothing, New BoundLabel(value), Nothing)
      Return New BoundReturnGosubStatement(label)
    End Function

    Private Function BindReturnStatement(syntax As ReturnStatementSyntax) As BoundStatement

      Dim expression = If(syntax.Expression Is Nothing, Nothing, BindExpression(syntax.Expression))

      If m_function Is Nothing Then
        If m_isScript Then
          ' Ignore because we allow both return with and without values.
          If expression Is Nothing Then
            expression = New BoundLiteralExpression("")
          End If
        ElseIf expression IsNot Nothing Then
          ' Main does not support return values.
          Diagnostics.ReportInvalidReturnWithValueInGlobalStatements(syntax.Expression.Location)
        End If
      Else
        If m_function.Type Is TypeSymbol.Nothing Then
          If expression IsNot Nothing Then
            Diagnostics.ReportInvalidReturnExpression(syntax.Expression.Location, m_function.Name)
          End If
        Else
          If expression Is Nothing Then
            Diagnostics.ReportMissingReturnExpression(syntax.ReturnKeyword.Location, m_function.Type)
          Else
            expression = BindConversion(syntax.Expression.Location, expression, m_function.Type)
          End If
        End If
      End If

      Return New BoundReturnStatement(expression)

    End Function

    Private Function BindRmDirStatement(syntax As RmDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundRmDirStatement(path)
    End Function

    Private Function BindSingleLineIfStatement(syntax As SingleLineIfStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        ElseIf syntax.ElseClause IsNot Nothing Then
          Diagnostics.ReportUnreachableCode(syntax.ElseClause.Statements)
        End If
      End If

      Dim statements As BoundStatement = Nothing
      If syntax.Statements.Kind = SyntaxKind.BlockStatement Then
        Dim child = syntax.Statements.GetChildren.First
        If child.Kind = SyntaxKind.ExpressionStatement Then
          child = child.GetChildren.First
          If child.Kind = SyntaxKind.LiteralExpression Then
            child = child.GetChildren.First
            If child.Kind = SyntaxKind.NumberToken Then
              Dim value = CType(child, SyntaxToken).Text
              If IsNumeric(value) Then
                ' An inferred GOTO... old school IF statement.
                value = $"{GOTO_LABEL_PREFIX}{value}"
                Dim label = New BoundLabel(value)
                Dim statement As BoundStatement = New BoundGotoStatement(label)
                statements = New BoundBlockStatement({statement}.ToImmutableArray)
              End If
            End If
          End If
        End If
      End If
      If statements Is Nothing Then
        statements = BindStatement(syntax.Statements)
      End If
      Dim elseStatement = If(syntax.ElseClause IsNot Nothing, BindStatement(syntax.ElseClause.Statements), Nothing)
      Return New BoundIfStatement(condition, statements, elseStatement)

    End Function

    Private Shared Function BindStopStatement(syntax As StopStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundStopStatement()
    End Function

    Private Shared Function BindSystemStatement(syntax As SystemStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundSystemStatement()
    End Function

    Private Function BindUnaryExpression(syntax As UnaryExpressionSyntax) As BoundExpression
      Dim boundOperand = BindExpression(syntax.Operand)
      If boundOperand.Type Is TypeSymbol.Error Then Return New BoundErrorExpression
      Dim boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperator Is Nothing Then
        Diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Location, syntax.OperatorToken.Text, boundOperand.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundUnaryExpression(boundOperator, boundOperand)
    End Function

    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax) As BoundStatement
      Dim isReadOnly = (syntax.KeywordToken.Kind = SyntaxKind.ConstKeyword)
      Dim type = BindAsClause(syntax.AsClause)
      Dim initializer = BindExpression(syntax.Initializer)
      Dim variableType = If(type, initializer.Type)
      Dim variable = BindVariableDeclaration(syntax.IdentifierToken, isReadOnly, variableType)
      Dim convertedInitializer = BindConversion(syntax.Initializer.Location, initializer, variableType)
      Return New BoundVariableDeclaration(variable, convertedInitializer)
    End Function

    Private Function BindVariableDeclaration(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol, Optional constant As BoundConstant = Nothing) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(m_function Is Nothing,
                        DirectCast(New GlobalVariableSymbol(name, isReadOnly, type, constant), VariableSymbol),
                        DirectCast(New LocalVariableSymbol(name, isReadOnly, type, constant), VariableSymbol))
      If [declare] AndAlso Not m_scope.TryDeclareVariable(variable) Then
        Diagnostics.ReportSymbolAlreadyDeclared(identifier.Location, name)
      End If
      Return variable
    End Function

    Private Function BindVariableReference(identifierToken As SyntaxToken) As VariableSymbol

      Dim name = identifierToken.Text
      Dim s = m_scope.TryLookupSymbol(name)

      If TypeOf s Is VariableSymbol Then
        Return TryCast(s, VariableSymbol)
      ElseIf s Is Nothing Then
        If Not OPTION_EXPLICIT Then
          Dim type = TypeSymbol.String
          If Not identifierToken.Text.EndsWith("$") Then
            type = TypeSymbol.Double
          End If
          Dim variable = BindVariableDeclaration(identifierToken, False, type)
          If variable Is Nothing Then
            Diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
            Return Nothing
          Else
            s = m_scope.TryLookupSymbol(name)
            Return TryCast(s, VariableSymbol)
          End If
        Else
          Diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
          Return Nothing
        End If
      Else
        Diagnostics.ReportNotAVariable(identifierToken.Location, name)
        Return Nothing
      End If

    End Function

    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        End If
      End If

      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Return New BoundWhileStatement(condition, statements, exitLabel, continueLabel)

    End Function

    Private Function DetermineVariableReference(identifierToken As SyntaxToken) As VariableSymbol

      Dim name = identifierToken.Text
      Dim s = m_scope.TryLookupSymbol(name)

      If TypeOf s Is VariableSymbol Then
        Return TryCast(s, VariableSymbol)
      Else
        Return Nothing
      End If

    End Function

    Private Shared Function LookupType(name As String) As TypeSymbol
      Select Case name.ToLower
        Case "any" : Return TypeSymbol.Any
        Case "object" : Return TypeSymbol.Object
        Case "udt" : Return TypeSymbol.Udt
        Case "datetime" : Return TypeSymbol.DateTime
        Case "boolean" : Return TypeSymbol.Boolean
        Case "byte" : Return TypeSymbol.Byte
        Case "SByte" : Return TypeSymbol.SByte
        Case "char" : Return TypeSymbol.Char
        Case "decimal" : Return TypeSymbol.Decimal
        Case "integer" : Return TypeSymbol.Integer
        Case "uinteger" : Return TypeSymbol.UInteger
        Case "long" : Return TypeSymbol.Long
        Case "ulong" : Return TypeSymbol.ULong
        Case "long64" : Return TypeSymbol.Long64
        Case "ulong64" : Return TypeSymbol.ULong64
        Case "single" : Return TypeSymbol.Single
        Case "double" : Return TypeSymbol.Double
        Case "string" : Return TypeSymbol.String
        Case Else
          Return Nothing
      End Select
    End Function

  End Class

End Namespace
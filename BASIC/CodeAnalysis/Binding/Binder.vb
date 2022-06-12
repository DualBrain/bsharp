Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Lowering
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private m_scope As BoundScope
    Private ReadOnly m_isScript As Boolean
    Private ReadOnly m_function As FunctionSymbol
    Private ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag
    Private ReadOnly m_loopStack As New Stack(Of (ExitLabel As BoundLabel, ContinueLabel As BoundLabel))
    Private m_labelCounter As Integer

    'Public ReadOnly Property Diagnostics As DiagnosticBag
    '  Get
    '    Return Diagnostics
    '  End Get
    'End Property

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

      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      For Each globalStatement In globalStatements
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
      Select Case syntax.Kind
        Case SyntaxKind.GotoStatement : Return BindGotoStatement(CType(syntax, GotoStatementSyntax))
        Case SyntaxKind.BlockStatement : Return BindBlockStatement(CType(syntax, BlockStatementSyntax))
        Case SyntaxKind.VariableDeclaration : Return BindVariableDeclaration(CType(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.SingleLineIfStatement : Return BindSingleLineIfStatement(CType(syntax, SingleLineIfStatementSyntax))
        Case SyntaxKind.IfStatement : Return BindIfStatement(CType(syntax, IfStatementSyntax))
        Case SyntaxKind.WhileStatement : Return BindWhileStatement(CType(syntax, WhileStatementSyntax))
        Case SyntaxKind.DoWhileStatement : Return BindDoWhileStatement(CType(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.DoUntilStatement : Return BindDoUntilStatement(CType(syntax, DoUntilStatementSyntax))
        Case SyntaxKind.ForStatement : Return BindForStatement(CType(syntax, ForStatementSyntax))
        Case SyntaxKind.ExitStatement : Return BindExitStatement(CType(syntax, ExitStatementSyntax))
        Case SyntaxKind.ContinueStatement : Return BindContinueStatement(CType(syntax, ContinueStatementSyntax))
        'Case SyntaxKind.SelectCaseStatement : Return BindSelectCaseStatement(CType(syntax, SelectCaseStatementSyntax))
        Case SyntaxKind.ReturnStatement : Return BindReturnStatement(CType(syntax, ReturnStatementSyntax))
        Case SyntaxKind.ExpressionStatement : Return BindExpressionStatement(CType(syntax, ExpressionStatementSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
    End Function

    Private Function BindGotoStatement(syntax As GotoStatementSyntax) As BoundStatement
      Dim label = New BoundLabel(syntax.IdentifierToken.Text)
      Return New BoundGotoStatement(label)
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

    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax) As BoundStatement
      Dim isReadOnly = (syntax.Keyword.Kind = SyntaxKind.ConstKeyword)
      Dim type = BindAsClause(syntax.AsClause)
      Dim initializer = BindExpression(syntax.Initializer)
      Dim variableType = If(type, initializer.Type)
      Dim variable = BindVariableDeclaration(syntax.Identifier, isReadOnly, variableType)
      Dim convertedInitializer = BindConversion(syntax.Initializer.Location, initializer, variableType)
      Return New BoundVariableDeclaration(variable, convertedInitializer)
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

    Private Function BindSingleLineIfStatement(syntax As SingleLineIfStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        ElseIf syntax.ElseClause IsNot Nothing Then
          Diagnostics.ReportUnreachableCode(syntax.ElseClause.Statements)
        End If
      End If

      Dim statements = BindStatement(syntax.Statements)
      Dim elseStatement = If(syntax.ElseClause IsNot Nothing, BindStatement(syntax.ElseClause.Statements), Nothing)
      Return New BoundIfStatement(condition, statements, elseStatement)

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

    Private Function BindDoWhileStatement(syntax As DoWhileStatementSyntax) As BoundStatement
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Dim expression = BindExpression(syntax.WhileClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.WhileClause.AtBeginning
      Return New BoundDoWhileStatement(statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Function BindDoUntilStatement(syntax As DoUntilStatementSyntax) As BoundStatement
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Dim expression = BindExpression(syntax.UntilClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.UntilClause.AtBeginning
      Return New BoundDoUntilStatement(statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.StartValue, TypeSymbol.Integer)
      Dim upperBound = BindExpression(syntax.EndValue, TypeSymbol.Integer)
      Dim stepper = If(syntax.Increment Is Nothing, Nothing, BindExpression(syntax.Increment, TypeSymbol.Integer))

      m_scope = New BoundScope(m_scope)

      Dim variable = BindVariableDeclaration(syntax.Identifier, True, TypeSymbol.Integer)
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim body = BindLoopBody(syntax.Statements, exitLabel, continueLabel)

      m_scope = m_scope.Parent

      Return New BoundForStatement(variable, lowerBound, upperBound, stepper, body, exitLabel, continueLabel)

    End Function

    'Private Function BindSelectCaseStatement(syntax As SelectCaseStatementSyntax) As BoundStatement

    '  Dim test = BindExpression(syntax.Test, TypeSymbol.Integer)

    '  Dim cases = ImmutableArray.CreateBuilder(Of BoundCaseStatement)
    '  For Each c In syntax.Cases
    '    Dim matches = ImmutableArray.CreateBuilder(Of BoundMatchStatement)
    '    For Each match In c.Matches
    '      Dim comparisonKind = match.ComparisonKind
    '      Dim expressionA = BindExpression(match.Expression)
    '      Dim expressionB = If(match.ExpressionTo IsNot Nothing, BindExpression(match.ExpressionTo), Nothing)
    '      matches.Add(New BoundMatchStatement(SyntaxFacts.GetText(comparisonKind), expressionA, expressionB))
    '    Next
    '    Dim statement = BindStatement(c.Statement) ' ImmutableArray.CreateBuilder(Of BoundStatement)
    '    'm_scope = New BoundScope(m_scope)
    '    'For Each statementSyntax In c.Statements
    '    '  Dim statement = BindStatement(statementSyntax)
    '    '  statements.Add(statement)
    '    'Next
    '    'm_scope = m_scope.Parent
    '    cases.Add(New BoundCaseStatement(matches.ToImmutableArray, statement))
    '  Next

    '  Dim elseStatement = BindStatement(syntax.CaseElseClause.Statement) 'ImmutableArray.CreateBuilder(Of BoundStatement)
    '  'If syntax.CaseElseClause IsNot Nothing Then
    '  '  m_scope = New BoundScope(m_scope)
    '  '  For Each statementSyntax In syntax.CaseElseClause.Statements
    '  '    Dim statement = BindStatement(statementSyntax)
    '  '    elseStatements.Add(statement)
    '  '  Next
    '  '  m_scope = m_scope.Parent
    '  'End If

    '  Return New BoundSelectCaseStatement(test, cases.ToImmutableArray, elseStatement)

    'End Function

    Private Function BindLoopBody(statements As StatementSyntax, ByRef exitLabel As BoundLabel, ByRef continueLabel As BoundLabel) As BoundStatement

      m_labelCounter += 1
      exitLabel = New BoundLabel($"exit{m_labelCounter}")
      continueLabel = New BoundLabel($"continue{m_labelCounter}")

      m_loopStack.Push((exitLabel, continueLabel))
      Dim boundBody = BindStatement(statements)
      m_loopStack.Pop()

      Return boundBody

    End Function

    Private Function BindExitStatement(syntax As ExitStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ExitKeyword.Location, syntax.ExitKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim exitLabel = m_loopStack.Peek().ExitLabel
      Return New BoundGotoStatement(exitLabel)

    End Function

    Private Function BindContinueStatement(syntax As ContinueStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ContinueKeyword.Location, syntax.ContinueKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim continueLabel = m_loopStack.Peek().ContinueLabel
      Return New BoundGotoStatement(continueLabel)

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

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, canBeVoid:=True)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return BindExpression(syntax.Expression)
    End Function

    Private Shared Function BindLiteralExpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      Return New BoundLiteralExpression(value)
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

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = BindExpression(syntax.Expression)

      Dim variable = BindVariableReference(syntax.IdentifierToken)
      If variable Is Nothing Then
        Return boundExpression
      End If

      If variable.IsReadOnly Then
        Diagnostics.ReportCannotAssign(syntax.EqualToken.Location, name)
      End If

      Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type)

      Return New BoundAssignmentExpression(variable, convertedExpression)

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

    Private Function BindCallExpression(syntax As CallExpressionSyntax) As BoundExpression

      Dim t = LookupType(syntax.Identifier.Text)
      If syntax.Arguments.Count = 1 AndAlso TypeOf t Is TypeSymbol Then
        Return BindConversion(syntax.Arguments(0), t, True)
      End If

      Dim boundArguments = ImmutableArray.CreateBuilder(Of BoundExpression)()

      For Each argument In syntax.Arguments
        Dim boundArgument = BindExpression(argument)
        boundArguments.Add(boundArgument)
      Next

      Dim symbol = m_scope.TryLookupSymbol(syntax.Identifier.Text)
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
        Diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
        Return Nothing
      Else
        Diagnostics.ReportNotAVariable(identifierToken.Location, name)
        Return Nothing
      End If

    End Function

    Private Shared Function LookupType(name As String) As TypeSymbol
      Select Case name
        Case "any" : Return TypeSymbol.Any
        Case "boolean" : Return TypeSymbol.Boolean
        Case "integer" : Return TypeSymbol.Integer
        Case "string", "cstr", "cstr$" : Return TypeSymbol.String
        Case Else
          Return Nothing
      End Select
    End Function

  End Class

End Namespace
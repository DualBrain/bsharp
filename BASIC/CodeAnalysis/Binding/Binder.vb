Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Lowering
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_function As FunctionSymbol
    Private m_scope As BoundScope

    Public Sub New(parent As BoundScope, [function] As FunctionSymbol)
      m_scope = New BoundScope(parent)
      m_function = [function]
      If [function] IsNot Nothing Then
        For Each p In [function].Parameters
          m_scope.TryDeclareVariable(p)
        Next
      End If
    End Sub

    Public Shared Function BindGlobalScope(previous As BoundGlobalScope, syntax As CompilationUnitSyntax) As BoundGlobalScope

      Dim parentScope = CreateParentScope(previous)
      'Dim binder = New Binder(parentScope)
      'Dim expression = binder.BindStatement(syntax.Statement)
      Dim binder = New Binder(parentScope, Nothing)

      For Each f In syntax.Members.OfType(Of FunctionDeclarationSyntax)
        binder.BindFunctionDeclaration(f)
      Next

      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)

      For Each globalStatement In syntax.Members.OfType(Of GlobalStatementSyntax)
        Dim statement = binder.BindStatement(globalStatement.Statement)
        statements.Add(statement)
      Next

      Dim functions = binder.m_scope.GetDeclaredFunctions
      Dim variables = binder.m_scope.GetDeclaredVariables
      Dim diagnostics = binder.Diagnostics.ToImmutableArray

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      'Return New BoundGlobalScope(previous, diagnostics, variables, expression)
      Return New BoundGlobalScope(previous, diagnostics, functions, variables, statements.ToImmutable)

    End Function

    Public Shared Function BindProgram(globalScope As BoundGlobalScope) As BoundProgram

      Dim parentScope = CreateParentScope(globalScope)

      Dim functionBodies = ImmutableDictionary.CreateBuilder(Of FunctionSymbol, BoundBlockStatement)()
      Dim diagnostics = ImmutableArray.CreateBuilder(Of Diagnostic)()

      Dim scope = globalScope

      While scope IsNot Nothing
        For Each f In scope.Functions
          Dim binder As New Binder(parentScope, f)
          Dim body = binder.BindStatement(f.Declaration.Statements)
          Dim loweredBody = Lowerer.Lower(body)
          functionBodies.Add(f, loweredBody)
          diagnostics.AddRange(binder.Diagnostics)
        Next
        scope = scope.Previous
      End While

      Dim statement = Lowerer.Lower(New BoundBlockStatement(globalScope.Statements))

      Return New BoundProgram(diagnostics.ToImmutable(), functionBodies.ToImmutable(), statement)

    End Function

    Private Sub BindFunctionDeclaration(syntax As FunctionDeclarationSyntax)

      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)()

      Dim seenParameterNames As New HashSet(Of String)

      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Text
        Dim parameterType = BindTypeClause(parameterSyntax.AsClause)
        If Not seenParameterNames.Add(parameterName) Then
          m_diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Span, parameterName)
        Else
          Dim parameter As New ParameterSymbol(parameterName, parameterType)
          parameters.Add(parameter)
        End If
      Next

      Dim type = If(BindTypeClause(syntax.AsClause), TypeSymbol.Nothing)

      If type IsNot TypeSymbol.Nothing Then
        m_diagnostics.XXX_ReportFunctionsAreUnsupported(syntax.AsClause.Span)
      End If

      Dim f As New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable(), type, syntax)
      If Not m_scope.TryDeclareFunction(f) Then
        m_diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Span, f.Name)
      End If

    End Sub

    Private Shared Function CreateParentScope(previous As BoundGlobalScope) As BoundScope
      Dim stack = New Stack(Of BoundGlobalScope)
      While previous IsNot Nothing
        stack.Push(previous)
        previous = previous.Previous
      End While
      Dim parent = CreateRootScope() 'As BoundScope = Nothing
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

    Public ReadOnly Property Diagnostics As DiagnosticBag
      Get
        Return m_diagnostics
      End Get
    End Property

    Private Function BindStatement(syntax As StatementSyntax) As BoundStatement
      Select Case syntax.Kind
        Case SyntaxKind.BlockStatement : Return BindBlockStatement(CType(syntax, BlockStatementSyntax))
        Case SyntaxKind.VariableDeclaration : Return BindVariableDeclaration(CType(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.SingleLineIfStatement : Return BindSingleLineIfStatement(CType(syntax, SingleLineIfStatementSyntax))
        Case SyntaxKind.MultiLineIfBlock : Return BindMultiLineIfBlock(CType(syntax, MultiLineIfBlock))
        Case SyntaxKind.WhileStatement : Return BindWhileStatement(CType(syntax, WhileStatementSyntax))
        Case SyntaxKind.DoWhileStatement : Return BindDoWhileStatement(CType(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.DoUntilStatement : Return BindDoUntilStatement(CType(syntax, DoUntilStatementSyntax))
        Case SyntaxKind.ForStatement : Return BindForStatement(CType(syntax, ForStatementSyntax))
        Case SyntaxKind.SelectCaseStatement : Return BindSelectCaseStatement(CType(syntax, SelectCaseStatementSyntax))
        Case SyntaxKind.ExpressionStatement : Return BindExpressionStatement(CType(syntax, ExpressionStatementSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
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
      Dim isReadOnly = (syntax.KeywordToken.Kind = SyntaxKind.ConstKeyword)
      Dim type = BindTypeClause(syntax.AsClause)
      Dim initializer = BindExpression(syntax.Initializer)
      'Dim variable = BindVariable(syntax.Identifier, isReadOnly, initializer.Type)
      Dim variableType = If(type, initializer.Type)
      Dim variable = BindVariable(syntax.Identifier, isReadOnly, variableType)
      Dim convertedInitializer = BindConversion(syntax.Initializer.Span, initializer, variableType)
      'Return New BoundVariableDeclaration(variable, initializer)
      Return New BoundVariableDeclaration(variable, convertedInitializer)
    End Function

    Private Function BindTypeClause(syntax As AsClauseSyntax) As TypeSymbol
      If syntax Is Nothing Then Return Nothing
      Dim type = LookupType(syntax.Identifier.Text)
      If type Is Nothing Then
        m_diagnostics.ReportUndefinedType(syntax.Identifier.Span, syntax.Identifier.Text)
      End If
      Return type
    End Function

    Private Function BindSingleLineIfStatement(syntax As SingleLineIfStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      'If condition.ConstantValue IsNot Nothing Then
      '  If Not CBool(condition.ConstantValue.Value) Then
      '    m_diagnostics.ReportUnreachableCode(syntax.ThenStatement)
      '  ElseIf syntax.ElseClause IsNot Nothing Then
      '    m_diagnostics.ReportUnreachableCode(syntax.ElseClause.ElseStatement)
      '  End If
      'End If

      Dim statements = BindStatement(syntax.Statements)
      Dim elseStatement = If(syntax.ElseClause IsNot Nothing, BindStatement(syntax.ElseClause.Statements), Nothing)
      Return New BoundIfStatement(condition, statements, elseStatement)

      'Dim ifCondition = BindExpression(syntax.Condition, TypeSymbol.Boolean)
      'Dim ifStatements = ImmutableArray.CreateBuilder(Of BoundStatement)
      'm_scope = New BoundScope(m_scope)
      'For Each statementSyntax In syntax.ThenStatement
      '  Dim statement = BindStatement(statementSyntax)
      '  ifStatements.Add(statement)
      'Next
      'm_scope = m_scope.Parent

      'Dim elseIfStatements = ImmutableArray.CreateBuilder(Of BoundElseIfStatement)
      'If syntax.ElseIfClause.Any Then
      '  For Each entry In syntax.ElseIfClauses
      '    Dim elseIfCondition = BindExpression(entry.Condition, TypeSymbol.Boolean)
      '    Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      '    m_scope = New BoundScope(m_scope)
      '    For Each statementSyntax In entry.Statements
      '      Dim statement = BindStatement(statementSyntax)
      '      statements.Add(statement)
      '    Next
      '    m_scope = m_scope.Parent
      '    elseIfStatements.Add(New BoundElseIfStatement(elseIfCondition, statements.ToImmutable))
      '  Next
      'End If

      'Dim elseStatements = ImmutableArray.CreateBuilder(Of BoundStatement)
      'If syntax.ElseClause IsNot Nothing Then
      '  m_scope = New BoundScope(m_scope)
      '  For Each statementSyntax In syntax.ElseClause.ElseStatement
      '    Dim statement = BindStatement(statementSyntax)
      '    elseStatements.Add(statement)
      '  Next
      '  m_scope = m_scope.Parent
      'End If

      'Return New BoundIfStatement(ifCondition, ifStatements.ToImmutable, elseIfStatements.ToImmutable, elseStatements.ToImmutable)

    End Function

    Private Function BindMultiLineIfBlock(syntax As MultiLineIfBlock) As BoundStatement

      Dim ifStatement = syntax.IfStatement
      Dim elseIfStatements = syntax.ElseIfStatements
      Dim elseStatement = syntax.ElseStatement

      'TODO: Need to handle ElseIf...

      Dim condition = BindExpression(ifStatement.Expression, TypeSymbol.Boolean)
      Dim thenStatement = BindStatement(ifStatement.Statements)
      Dim elseClause = If(elseStatement IsNot Nothing, BindStatement(elseStatement.Statements), Nothing)
      Return New BoundIfStatement(condition, thenStatement, elseClause)

    End Function

    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement
      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)
      Dim statement = BindStatement(syntax.Body)
      Return New BoundWhileStatement(condition, statement)
    End Function

    Private Function BindDoWhileStatement(syntax As DoWhileStatementSyntax) As BoundStatement
      Dim body = BindStatement(syntax.Body)
      Dim expression = BindExpression(syntax.WhileClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.WhileClause.AtBeginning
      Return New BoundDoWhileStatement(body, expression, atBeginning)
    End Function

    Private Function BindDoUntilStatement(syntax As DoUntilStatementSyntax) As BoundStatement
      Dim body = BindStatement(syntax.Body)
      Dim expression = BindExpression(syntax.UntilClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.UntilClause.AtBeginning
      Return New BoundDoUntilStatement(body, expression, atBeginning)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.startValue, TypeSymbol.Integer)
      Dim upperBound = BindExpression(syntax.endValue, TypeSymbol.Integer)
      Dim stepper = If(syntax.Increment Is Nothing, Nothing, BindExpression(syntax.Increment, TypeSymbol.Integer))

      m_scope = New BoundScope(m_scope)

      'Dim name = syntax.Variable.Text
      'Dim variable = New VariableSymbol(name, True, TypeSymbol.Integer) ' GetType(Integer))
      'If Not m_scope.TryDeclare(variable) Then
      '  m_diagnostics.ReportVariableAlreadyDeclared(syntax.Variable.Span, name)
      'End If

      Dim variable = BindVariable(syntax.Identifier, True, TypeSymbol.Integer)

      Dim body = BindStatement(syntax.Statements)

      m_scope = m_scope.Parent

      Return New BoundForStatement(variable, lowerBound, upperBound, stepper, body)

    End Function

    Private Function BindSelectCaseStatement(syntax As SelectCaseStatementSyntax) As BoundStatement

      Dim test = BindExpression(syntax.Test, TypeSymbol.Integer)

      Dim cases = ImmutableArray.CreateBuilder(Of BoundCaseStatement)
      For Each c In syntax.Cases
        Dim matches = ImmutableArray.CreateBuilder(Of BoundMatchStatement)
        For Each match In c.Matches
          Dim comparisonKind = match.ComparisonKind
          Dim expressionA = BindExpression(match.Expression)
          Dim expressionB = If(match.ExpressionTo IsNot Nothing, BindExpression(match.ExpressionTo), Nothing)
          matches.Add(New BoundMatchStatement(SyntaxFacts.GetText(comparisonKind), expressionA, expressionB))
        Next
        Dim statement = BindStatement(c.Statement) ' ImmutableArray.CreateBuilder(Of BoundStatement)
        'm_scope = New BoundScope(m_scope)
        'For Each statementSyntax In c.Statements
        '  Dim statement = BindStatement(statementSyntax)
        '  statements.Add(statement)
        'Next
        'm_scope = m_scope.Parent
        cases.Add(New BoundCaseStatement(matches.ToImmutableArray, statement))
      Next

      Dim elseStatement = BindStatement(syntax.CaseElseClause.Statement) 'ImmutableArray.CreateBuilder(Of BoundStatement)
      'If syntax.CaseElseClause IsNot Nothing Then
      '  m_scope = New BoundScope(m_scope)
      '  For Each statementSyntax In syntax.CaseElseClause.Statements
      '    Dim statement = BindStatement(statementSyntax)
      '    elseStatements.Add(statement)
      '  Next
      '  m_scope = m_scope.Parent
      'End If

      Return New BoundSelectCaseStatement(test, cases.ToImmutableArray, elseStatement)

    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, canBeVoid:=True)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Return BindConversion(syntax, targetType)
      'Dim result = BindExpression(syntax)
      'If targetType IsNot TypeSymbol.Error AndAlso
      '   result.Type IsNot TypeSymbol.Error AndAlso
      '   result.Type IsNot targetType Then
      '  m_diagnostics.ReportCannotConvert(syntax.Span, result.Type, targetType)
      'End If
      'Return result
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, Optional canBeVoid As Boolean = False) As BoundExpression
      Dim result = BindExpressionInternal(syntax)
      If Not canBeVoid AndAlso result.Type Is TypeSymbol.Nothing Then
        m_diagnostics.ReportExpressionMustHaveValue(syntax.Span)
        Return New BoundErrorExpression
      End If
      Return result
    End Function

    Private Function BindExpressionInternal(syntax As ExpressionSyntax) As BoundExpression
      Select Case syntax.Kind
        Case SyntaxKind.ParenthesizedExpression : Return BindParenthesizedExpression(CType(syntax, ParenthesizedExpressionSyntax))
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

    Private Function BindParenthesizedExpression(syntax As ParenthesizedExpressionSyntax) As BoundExpression
      Return BindExpression(syntax.Expression)
    End Function

    Private Shared Function BindLiteralExpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      Return New BoundLiteralExpression(value)
    End Function

    Private Function BindNameExpression(syntax As NameExpressionSyntax) As BoundExpression
      Dim name = syntax.IdentifierToken.Text
      If syntax.IdentifierToken.IsMissing Then 'String.IsNullOrEmpty(name) Then
        ' This means the token was inserted by the parser. We already
        ' reported error so we can just return an error expression.
        Return New BoundErrorExpression ' BoundLiteralExpression(0)
      End If
      Dim variable As VariableSymbol = Nothing
      m_scope.TryLookupVariable(name, variable)
      If variable Is Nothing Then
        m_diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return New BoundErrorExpression ' BoundLiteralExpression(0)
      End If
      Return New BoundVariableExpression(variable)
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim name = syntax.IdentifierToken.Text
      Dim boundExpression = BindExpression(syntax.Expression)

      Dim variable As VariableSymbol = Nothing
      If Not m_scope.TryLookupVariable(name, variable) Then
        m_diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return boundExpression
      End If

      If variable.IsReadOnly Then
        m_diagnostics.ReportCannotAssign(syntax.EqualsToken.Span, name)

      End If

      'If boundExpression.Type IsNot variable.Type Then
      '  m_diagnostics.ReportCannotConvert(syntax.IdentifierToken.Span, boundExpression.Type, variable.Type)
      '  Return boundExpression
      'End If

      Dim convertedExpression = BindConversion(syntax.Expression.Span, boundExpression, variable.Type)

      'Return New BoundAssignmentExpression(variable, boundExpression)
      Return New BoundAssignmentExpression(variable, convertedExpression)

    End Function

    Private Function BindUnaryExpression(syntax As UnaryExpressionSyntax) As BoundExpression
      Dim boundOperand = BindExpression(syntax.Operand)
      If boundOperand.Type Is TypeSymbol.Error Then Return New BoundErrorExpression
      Dim boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperator Is Nothing Then
        m_diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken.Text, boundOperand.Type)
        Return New BoundErrorExpression 'boundOperand
      End If
      Return New BoundUnaryExpression(boundOperator, boundOperand)
    End Function

    Private Function BindBinaryExpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = BindExpression(syntax.Left)
      Dim boundRight = BindExpression(syntax.Right)
      If boundLeft.Type Is TypeSymbol.Error OrElse boundRight.Type Is TypeSymbol.Error Then Return New BoundErrorExpression
      Dim boundOperator = BoundBinaryOperator.Bind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperator Is Nothing Then
        m_diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken.Text, boundLeft.Type, boundRight.Type)
        Return New BoundErrorExpression 'boundLeft
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperator, boundRight)
    End Function

    Private Function BindCallExpression(syntax As CallExpressionSyntax) As BoundExpression

      If syntax.Arguments.Count = 1 AndAlso TypeOf LookupType(syntax.Identifier.Text) Is TypeSymbol Then
        Dim type = CType(LookupType(syntax.Identifier.Text), TypeSymbol)
        Return BindConversion(syntax.Arguments(0), type, True)
      End If

      Dim boundArguments = ImmutableArray.CreateBuilder(Of BoundExpression)()

      For Each argument In syntax.Arguments
        Dim boundArgument = BindExpression(argument)
        boundArguments.Add(boundArgument)
      Next

      Dim f As FunctionSymbol = Nothing
      If Not m_scope.TryLookupFunction(syntax.Identifier.Text, f) Then
        m_diagnostics.ReportUndefinedFunction(syntax.Identifier.Span, syntax.Identifier.Text)
        Return New BoundErrorExpression
      End If

      If syntax.Arguments.Count <> f.Parameters.Length Then
        m_diagnostics.ReportWrongArgumentCount(syntax.Span, f.Name, f.Parameters.Length, syntax.Arguments.Count)
        Return New BoundErrorExpression
      End If

      For i = 0 To syntax.Arguments.Count - 1
        Dim argument = boundArguments(i)
        Dim parameter = f.Parameters(i)
        If argument.Type IsNot parameter.Type Then
          m_diagnostics.ReportWrongArgumentType(syntax.Arguments(i).Span, parameter.Name, parameter.Type, argument.Type)
          Return New BoundErrorExpression
        End If
      Next

      Return New BoundCallExpression(f, boundArguments.ToImmutable())

    End Function

    Private Function BindConversion(syntax As ExpressionSyntax, type As TypeSymbol, Optional allowExplicit As Boolean = False) As BoundExpression
      Dim expression = BindExpression(syntax)
      Return BindConversion(syntax.Span, expression, type, allowExplicit)
    End Function

    Private Function BindConversion(diagnosticSpan As TextSpan, expression As BoundExpression, type As TypeSymbol, Optional allowExplicit As Boolean = False) As BoundExpression
      Dim conversion = Binding.Conversion.Classify(expression.Type, type)
      If Not conversion.Exists Then
        If expression.Type IsNot TypeSymbol.Error AndAlso type IsNot TypeSymbol.Error Then
          m_diagnostics.ReportCannotConvert(diagnosticSpan, expression.Type, type)
        End If
        Return New BoundErrorExpression
      End If
      If Not allowExplicit AndAlso conversion.IsExplicit Then
        m_diagnostics.ReportCannotConvertImplicitly(diagnosticSpan, expression.Type, type)
      End If
      If conversion.IsIdentity Then Return expression
      Return New BoundConversionExpression(type, expression)
    End Function

    Private Function BindVariable(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim decl = Not identifier.IsMissing
      'Dim variable = New VariableSymbol(name, isReadOnly, type)
      Dim variable = If(m_function Is Nothing, CType(New GlobalVariableSymbol(name, isReadOnly, type), VariableSymbol), New LocalVariableSymbol(name, isReadOnly, type))
      If decl AndAlso Not m_scope.TryDeclareVariable(variable) Then
        m_diagnostics.ReportSymbolAlreadyDeclared(identifier.Span, name)
      End If
      Return variable
    End Function

    Private Shared Function LookupType(name As String) As TypeSymbol
      Select Case name
        Case "boolean" : Return TypeSymbol.Boolean
        Case "integer" : Return TypeSymbol.Integer
        Case "string" : Return TypeSymbol.String
        Case Else
          Return Nothing
      End Select
    End Function

    'Private Shared Function BindUnaryOperatorKind(kind As SyntaxKind, operandType As Type) As BoundUnaryOperatorKind?

    '  If operandType = GetType(Integer) Then
    '    Select Case kind
    '      Case SyntaxKind.PlusToken : Return BoundUnaryOperatorKind.Identity
    '      Case SyntaxKind.MinusToken : Return BoundUnaryOperatorKind.Negation
    '      Case Else
    '    End Select
    '  End If

    '  If operandType = GetType(Boolean) Then
    '    Select Case kind
    '      Case SyntaxKind.NotKeyword : Return BoundUnaryOperatorKind.LogicalNegation
    '      Case Else
    '    End Select
    '  End If

    '  Return Nothing

    'End Function

    'Private Shared Function BindBinaryOperatorKind(kind As SyntaxKind, leftType As Type, rightType As Type) As BoundBinaryOperatorKind?

    '  If leftType = GetType(Integer) AndAlso rightType = GetType(Integer) Then
    '    Select Case kind
    '      Case SyntaxKind.PlusToken : Return BoundBinaryOperatorKind.Addition
    '      Case SyntaxKind.MinusToken : Return BoundBinaryOperatorKind.Subtraction
    '      Case SyntaxKind.StarToken : Return BoundBinaryOperatorKind.Multiplication
    '      Case SyntaxKind.SlashToken : Return BoundBinaryOperatorKind.Division
    '      Case SyntaxKind.BackslashToken : Return BoundBinaryOperatorKind.IntegerDivision

    '      Case SyntaxKind.ModKeyword : Return BoundBinaryOperatorKind.ModOperation
    '      Case SyntaxKind.AndKeyword : Return BoundBinaryOperatorKind.AndOperation
    '      Case SyntaxKind.OrKeyword : Return BoundBinaryOperatorKind.OrOperation
    '      Case SyntaxKind.XorKeyword : Return BoundBinaryOperatorKind.XorOperation
    '      Case SyntaxKind.EqvKeyword : Return BoundBinaryOperatorKind.EqvOperation
    '      Case SyntaxKind.ImpKeyword : Return BoundBinaryOperatorKind.ImpOperation

    '      Case Else
    '    End Select
    '  End If

    '  If leftType = GetType(Boolean) AndAlso rightType = GetType(Boolean) Then
    '    Select Case kind
    '      Case SyntaxKind.AndKeyword : Return BoundBinaryOperatorKind.LogicalAnd
    '      Case SyntaxKind.OrKeyword : Return BoundBinaryOperatorKind.LogicalOr
    '      Case SyntaxKind.AndAlsoKeyword : Return BoundBinaryOperatorKind.LogicalAndAlso
    '      Case SyntaxKind.OrElseKeyword : Return BoundBinaryOperatorKind.LogicalOrElse
    '      Case SyntaxKind.EqvKeyword : Return BoundBinaryOperatorKind.LogicalEqv
    '      Case SyntaxKind.ImpKeyword : Return BoundBinaryOperatorKind.LogicalImp
    '      Case Else
    '    End Select
    '  End If

    '  Return Nothing

    'End Function

  End Class

End Namespace
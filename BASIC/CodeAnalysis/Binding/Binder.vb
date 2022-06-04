﻿Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private m_scope As BoundScope

    Public Sub New(parent As BoundScope)
      m_scope = New BoundScope(parent)
    End Sub

    Public Shared Function BindGlobalScope(previous As BoundGlobalScope, syntax As CompilationUnitSyntax) As BoundGlobalScope
      Dim parentScope = CreateParentScopes(previous)
      Dim binder = New Binder(parentScope)
      Dim expression = binder.BindStatement(syntax.Statement)
      Dim variables = binder.m_scope.GetDeclaredVariables
      Dim diagnostics = binder.Diagnostics.ToImmutableArray

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      Return New BoundGlobalScope(previous, diagnostics, variables, expression)
    End Function

    Private Shared Function CreateParentScopes(previous As BoundGlobalScope) As BoundScope
      Dim stack = New Stack(Of BoundGlobalScope)
      While previous IsNot Nothing
        stack.Push(previous)
        previous = previous.Previous
      End While
      Dim parent As BoundScope = Nothing
      While stack.Count > 0
        previous = stack.Pop
        Dim scope = New BoundScope(parent)
        For Each v In previous.Variables
          scope.TryDeclare(v)
        Next
        parent = scope
      End While
      Return parent
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
        Case SyntaxKind.IfStatement : Return BindIfStatement(CType(syntax, IfStatementSyntax))
        Case SyntaxKind.WhileStatement : Return BindWhileStatement(CType(syntax, WhileStatementSyntax))
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
      Dim initializer = BindExpression(syntax.Initializer)

      Dim variable = BindVariable(syntax.Identifier, isReadOnly, initializer.Type)

      Return New BoundVariableDeclaration(variable, initializer)

    End Function

    Private Function BindIfStatement(syntax As IfStatementSyntax) As BoundStatement

      Dim ifCondition = BindExpression(syntax.Condition, TypeSymbol.Boolean)
      Dim ifStatements = ImmutableArray.CreateBuilder(Of BoundStatement)
      If syntax.TrueBlock.Any Then
        m_scope = New BoundScope(m_scope)
        For Each statementSyntax In syntax.TrueBlock
          Dim statement = BindStatement(statementSyntax)
          ifStatements.Add(statement)
        Next
        m_scope = m_scope.Parent
      End If

      Dim elseIfStatements = ImmutableArray.CreateBuilder(Of BoundElseIfStatement)
      If syntax.ElseIfClauses.Any Then
        For Each entry In syntax.ElseIfClauses
          Dim elseIfCondition = BindExpression(entry.Condition, TypeSymbol.Boolean)
          Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
          m_scope = New BoundScope(m_scope)
          For Each statementSyntax In entry.Statements
            Dim statement = BindStatement(statementSyntax)
            statements.Add(statement)
          Next
          m_scope = m_scope.Parent
          elseIfStatements.Add(New BoundElseIfStatement(elseIfCondition, statements.ToImmutable))
        Next
      End If

      Dim elseStatements = ImmutableArray.CreateBuilder(Of BoundStatement)
      If syntax.ElseClause IsNot Nothing Then
        m_scope = New BoundScope(m_scope)
        For Each statementSyntax In syntax.ElseClause.ElseBlock
          Dim statement = BindStatement(statementSyntax)
          elseStatements.Add(statement)
        Next
        m_scope = m_scope.Parent
      End If

      Return New BoundIfStatement(ifCondition, ifStatements.ToImmutable, elseIfStatements.ToImmutable, elseStatements.ToImmutable)

    End Function

    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement
      Dim condition = BindExpression(syntax.Condition, TypeSymbol.Boolean)
      Dim statement = BindStatement(syntax.Body)
      Return New BoundWhileStatement(condition, statement)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.LowerBound, TypeSymbol.Integer)
      Dim upperBound = BindExpression(syntax.UpperBound, TypeSymbol.Integer)
      Dim stepper = If(syntax.Stepper Is Nothing, Nothing, BindExpression(syntax.Stepper, TypeSymbol.Integer))

      m_scope = New BoundScope(m_scope)

      'Dim name = syntax.Variable.Text
      'Dim variable = New VariableSymbol(name, True, TypeSymbol.Integer) ' GetType(Integer))
      'If Not m_scope.TryDeclare(variable) Then
      '  m_diagnostics.ReportVariableAlreadyDeclared(syntax.Variable.Span, name)
      'End If

      Dim variable = BindVariable(syntax.Identifier, True, TypeSymbol.Integer)

      Dim body = BindStatement(syntax.Body)

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
        Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
        m_scope = New BoundScope(m_scope)
        For Each statementSyntax In c.Statements
          Dim statement = BindStatement(statementSyntax)
          statements.Add(statement)
        Next
        m_scope = m_scope.Parent
        cases.Add(New BoundCaseStatement(matches.ToImmutableArray, statements.ToImmutableArray))
      Next

      Dim elseStatements = ImmutableArray.CreateBuilder(Of BoundStatement)
      If syntax.CaseElseClause IsNot Nothing Then
        m_scope = New BoundScope(m_scope)
        For Each statementSyntax In syntax.CaseElseClause.Statements
          Dim statement = BindStatement(statementSyntax)
          elseStatements.Add(statement)
        Next
        m_scope = m_scope.Parent
      End If

      Return New BoundSelectCaseStatement(test, cases.ToImmutableArray, elseStatements.ToImmutableArray)

    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Dim result = BindExpression(syntax)
      If targetType IsNot TypeSymbol.Error AndAlso
         result.Type IsNot TypeSymbol.Error AndAlso
         result.Type IsNot targetType Then
        m_diagnostics.ReportCannotConvert(syntax.Span, result.Type, targetType)
      End If
      Return result
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax) As BoundExpression
      Select Case syntax.Kind
        Case SyntaxKind.ParenthesizedExpression : Return BindParenthesizedExpression(CType(syntax, ParenthesizedExpressionSyntax))
        Case SyntaxKind.LiteralExpression : Return BindLiteralExpression(CType(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.NameExpression : Return BindNameExpression(CType(syntax, NameExpressionSyntax))
        Case SyntaxKind.AssignmentExpression : Return BindAssignmentExpression(CType(syntax, AssignmentExpressionSyntax))
        Case SyntaxKind.UnaryExpression : Return BindUnaryExpression(CType(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression : Return BindBinaryExpression(CType(syntax, BinaryExpressionSyntax))
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
      m_scope.TryLookup(name, variable)
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
      If Not m_scope.TryLookup(name, variable) Then
        m_diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name)
        Return boundExpression
      End If

      If variable.IsReadOnly Then
        m_diagnostics.ReportCannotAssign(syntax.EqualsToken.Span, name)

      End If

      If boundExpression.Type IsNot variable.Type Then
        m_diagnostics.ReportCannotConvert(syntax.IdentifierToken.Span, boundExpression.Type, variable.Type)
        Return boundExpression
      End If

      Return New BoundAssignmentExpression(variable, boundExpression)

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

    Private Function BindVariable(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim decl = Not identifier.IsMissing
      Dim variable = New VariableSymbol(name, isReadOnly, type)
      If decl AndAlso Not m_scope.TryDeclare(variable) Then
        m_diagnostics.ReportVariableAlreadyDeclared(identifier.Span, name)
      End If
      Return variable
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
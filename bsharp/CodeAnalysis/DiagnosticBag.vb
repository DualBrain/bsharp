Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax
Imports Bsharp.CodeAnalysis.Text
Imports Mono.Cecil

Namespace Bsharp.CodeAnalysis

  Friend NotInheritable Class DiagnosticBag
    Implements IEnumerable(Of Diagnostic)

    Private ReadOnly m_diagnostics As New List(Of Diagnostic)

    Public Function GetEnumerator() As IEnumerator(Of Diagnostic) Implements IEnumerable(Of Diagnostic).GetEnumerator
      Return m_diagnostics.GetEnumerator
    End Function

    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
      Return GetEnumerator()
    End Function

    Private Sub ReportError(location As TextLocation, message As String)
      Dim d = Diagnostic.Error(location, message)
      m_diagnostics.Add(d)
    End Sub

    Private Sub ReportWarning(location As TextLocation, message As String)
      Dim d = Diagnostic.Warning(location, message)
      m_diagnostics.Add(d)
    End Sub

    Public Sub AddRange(diagnostics As IEnumerable(Of Diagnostic))
      m_diagnostics.AddRange(diagnostics)
    End Sub

    Public Sub Concat(diagnostics As DiagnosticBag)
      m_diagnostics.Concat(diagnostics.m_diagnostics)
    End Sub

    Public Sub ReportInvalidNumber(location As TextLocation, text As String, type As TypeSymbol)
      ReportError(location, $"The number {text} isn't valid {type}.")
    End Sub

    Public Sub ReportBadCharacter(location As TextLocation, character As Char)
      ReportError(location, $"Bad character input: '{character}'.")
    End Sub

    Public Sub ReportUnterminatedString(location As TextLocation)
      ReportError(location, $"Unterminated string literal.")
    End Sub

    Public Sub ReportUnterminatedMultiLineComment(location As TextLocation)
      ReportError(location, $"Unterminated mult-line comment.")
    End Sub

    Public Sub ReportUnexpectedToken(location As TextLocation, actualKind As SyntaxKind, expectedKind As SyntaxKind)
      ReportError(location, $"Unexpected token <{actualKind}>, expected <{expectedKind}>.")
    End Sub

    Public Sub ReportUndefinedUnaryOperator(location As TextLocation, operatorText As String, operandType As TypeSymbol)
      ReportError(location, $"Unary operator '{operatorText}' is not defined for type '{operandType}'.")
    End Sub

    Public Sub ReportUndefinedBinaryOperator(location As TextLocation, operatorText As String, leftType As TypeSymbol, rightType As TypeSymbol)
      ReportError(location, $"Binary operator '{operatorText}' is not defined for type '{leftType}' and '{rightType}'.")
    End Sub

    Public Sub ReportParameterAlreadyDeclared(location As TextLocation, parameterName As String)
      ReportError(location, $"A parameter with the name '{parameterName}' already exists.")
    End Sub

    'Public Sub ReportUndefinedName(location As TextLocation, name As String)
    Public Sub ReportUndefinedVariable(location As TextLocation, name As String)
      ReportError(location, $"Variable '{name}' doesn't exist.")
    End Sub

    Public Sub ReportNotAVariable(location As TextLocation, name As String)
      ReportError(location, $"'{name}' is not a variable.")
    End Sub

    Friend Sub ReportUndefinedType(location As TextLocation, name As String)
      ReportError(location, $"Type '{name}' doesn't exist.")
    End Sub

    Public Sub ReportCannotConvert(location As TextLocation, fromType As TypeSymbol, toType As TypeSymbol)
      ReportError(location, $"Cannot convert type '{fromType}' to '{toType}'.")
    End Sub

    Public Sub ReportCannotConvertImplicitly(location As TextLocation, fromType As TypeSymbol, toType As TypeSymbol)
      ReportError(location, $"Cannot convert type '{fromType}' to '{toType}'. An explicit conversion exists (are you missing a cast?)")
    End Sub

    Public Sub ReportSymbolAlreadyDeclared(location As TextLocation, name As String)
      ReportError(location, $"'{name}' is already declared.")
    End Sub

    Public Sub ReportCannotAssign(location As TextLocation, name As String)
      ReportError(location, $"Variable '{name}' is read-only and cannot be assigned to.")
    End Sub

    Public Sub ReportUndefinedFunction(location As TextLocation, name As String)
      ReportError(location, $"Function '{name}' doesn't exist.")
    End Sub

    Public Sub ReportExpressionMustHaveValue(location As TextLocation)
      ReportError(location, "Expression must have a value.")
    End Sub

    Public Sub ReportNotAFunction(location As TextLocation, name As String)
      ReportError(location, $"'{name}' is not a function.")
    End Sub

    Public Sub ReportWrongArgumentCount(location As TextLocation, name As String, expectedCount As Integer, actualCount As Integer)
      ReportError(location, $"Function '{name}' requires {expectedCount} arguments but was given {actualCount}.")
    End Sub

    Public Sub ReportWrongArgumentType(location As TextLocation, name As String, expectedType As TypeSymbol, actualType As TypeSymbol)
      ReportError(location, $"Parameter '{name}' requires a value of type '{expectedType}' but was given a value of type '{actualType}'.")
    End Sub

    Public Sub ReportInvalidBreakOrContinue(location As TextLocation, text As String)
      ReportError(location, $"The keyword '{text}' can only be used inside of loops.")
    End Sub

    Public Sub ReportAllPathsMustReturn(location As TextLocation)
      ReportError(location, "Not all code paths return a value.")
    End Sub

    Public Sub ReportInvalidReturnExpression(location As TextLocation, functionName As String)
      ReportError(location, $"Since the function '{functionName}' does not return a value, the 'return' keyword cannot be followed by an expression.")
    End Sub

    Public Sub ReportInvalidReturnWithValueInGlobalStatements(location As TextLocation)
      ReportError(location, $"The 'return' keyword cannot be followed by an expression in global statements.")
    End Sub

    Public Sub ReportMissingReturnExpression(location As TextLocation, returnType As TypeSymbol)
      ReportError(location, $"An expression of type '{returnType}' is expected.")
    End Sub

    Public Sub ReportInvalidExpressionStatement(location As TextLocation)
      ReportError(location, "Only assignment and call epxressions can be used as a statement.")
    End Sub

    Public Sub ReportOnlyOneFileCanHaveGlobalStatements(location As TextLocation)
      ReportError(location, "At most one file can have global statements.")
    End Sub

    Public Sub ReportMainMustHaveCorrectSignature(location As TextLocation)
      ReportError(location, "main must not take arguments and not return anything.")
    End Sub

    Public Sub ReportCannotMixMainAndGlobalStatements(location As TextLocation)
      ReportError(location, "Cannot declare main function when global statements are used.")
    End Sub

    Public Sub ReportInvalidReference(path As String)
      ReportError(Nothing, $"The reference is not a valid .NET assembly: '{path}'")
    End Sub

    Public Sub ReportRequiredTypeNotFound(internalName As String, metadataName As String)
      ReportError(Nothing, If(internalName Is Nothing,
                         $"The required type '{metadataName}' cannot be resolved among the given references.",
                         $"The required type '{internalName}' ('{metadataName}') cannot be resolved among the given references."))
    End Sub

    Public Sub ReportRequiredTypeAmbiguous(internalName As String, metadataName As String, foundTypes() As TypeDefinition)
      Dim assemblyNames = foundTypes.Select(Function(t) t.Module.Assembly.Name.Name)
      Dim assemblyNameList = String.Join(", ", assemblyNames)
      ReportError(Nothing, If(internalName Is Nothing,
                         $"The required type '{metadataName}' was found in multiple references: {assemblyNameList}",
                         $"The required type '{internalName}' ('{metadataName}') was found in multiple references: {assemblyNameList}"))
    End Sub

    Public Sub ReportRequiredMethodNotFound(typeName As String, methodName As String, parameterTypeNames() As String)
      Dim parameterTypeNameList = String.Join(", ", parameterTypeNames)
      ReportError(Nothing, $"The required method '{typeName}.{methodName}({parameterTypeNameList})' cann be resolved among the given references.")
    End Sub

    Public Sub ReportUnreachableCode(location As TextLocation)
      ReportError(location, $"Unreachable code detected.")
    End Sub

    Public Sub ReportUnreachableCode(node As SyntaxNode)
      Select Case node.Kind
        Case SyntaxKind.BlockStatement
          Dim firstStatement = CType(node, BlockStatementSyntax).Statements.FirstOrDefault()
          ' Report just for non empty blocks.
          If firstStatement IsNot Nothing Then ReportUnreachableCode(firstStatement)
        Case SyntaxKind.VariableDeclarationStatement
          ReportUnreachableCode(CType(node, VariableDeclarationSyntax).KeywordToken.Location)
        Case SyntaxKind.IfStatement
          ReportUnreachableCode(CType(node, IfStatementSyntax).IfKeyword.Location)
        Case SyntaxKind.WhileStatement
          ReportUnreachableCode(CType(node, WhileStatementSyntax).WhileKeyword.Location)
        Case SyntaxKind.DoWhileStatement
          ReportUnreachableCode(CType(node, DoWhileStatementSyntax).DoKeyword.Location)
        Case SyntaxKind.ForStatement
          ReportUnreachableCode(CType(node, ForStatementSyntax).ForKeyword.Location)
        Case SyntaxKind.ExitStatement
          ReportUnreachableCode(CType(node, ExitStatementSyntax).ExitKeyword.Location)
        Case SyntaxKind.ContinueStatement
          ReportUnreachableCode(CType(node, ContinueStatementSyntax).ContinueKeyword.Location)
        Case SyntaxKind.ReturnStatement
          ReportUnreachableCode(CType(node, ReturnStatementSyntax).ReturnKeyword.Location)
        Case SyntaxKind.ExpressionStatement
          ReportUnreachableCode(CType(node, ExpressionStatementSyntax).Expression)
        Case SyntaxKind.CallExpression
          ReportUnreachableCode(CType(node, CallExpressionSyntax).Identifier.Location)
        Case Else
          Throw New Exception($"Unexpected syntax {node.Kind}")
      End Select
    End Sub

  End Class

End Namespace
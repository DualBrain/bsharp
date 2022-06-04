Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis

  Friend NotInheritable Class DiagnosticBag
    Implements IEnumerable(Of Diagnostic)

    Private ReadOnly m_diagnostics As New List(Of Diagnostic)

    Private Sub Report(span As TextSpan, message As String)
      Dim diagnostic = New Diagnostic(span, message)
      m_diagnostics.Add(diagnostic)
    End Sub

    Public Function GetEnumerator() As IEnumerator(Of Diagnostic) Implements IEnumerable(Of Diagnostic).GetEnumerator
      Return m_diagnostics.GetEnumerator()
    End Function

    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
      Return GetEnumerator()
    End Function

    Public Sub AddRange(diagnostics As DiagnosticBag)
      m_diagnostics.AddRange(diagnostics.m_diagnostics)
    End Sub

    Public Sub ReportInvalidNumber(span As TextSpan, text As String, type As Type)
      Dim message = $"The number {text} isn't a valid {type}."
      Report(span, message)
    End Sub

    Public Sub ReportBadCharacter(position As Integer, character As Char)
      Dim message = $"Bad character input: '{character}'."
      Report(New TextSpan(position, 1), message)
    End Sub

    Public Sub ReportUnexpectedToken(span As TextSpan, actualKind As SyntaxKind, expectedKind As SyntaxKind)
      Dim message = $"Unexpected token <{actualKind}>, expected <{expectedKind}>."
      Report(span, message)
    End Sub

    Public Sub ReportUndefinedUnaryOperator(span As TextSpan, operatorText As String, operandType As TypeSymbol)
      Dim message = $"Unary operator '{operatorText}' is not defined for type '{operandType}'."
      Report(span, message)
    End Sub

    Public Sub ReportUndefinedBinaryOperator(span As TextSpan, operatorText As String, leftType As TypeSymbol, rightType As TypeSymbol)
      Dim message = $"Binary operator '{operatorText}' is not defined for type '{leftType}' and '{rightType}'."
      Report(span, message)
    End Sub

    Public Sub ReportUndefinedName(span As TextSpan, name As String)
      Dim message = $"Variable '{name}' doesn't exist."
      Report(span, message)
    End Sub

    Public Sub ReportSymbolAlreadyDeclared(span As TextSpan, name As String)
      Dim message = $"'{name}' is already declared."
      Report(span, message)
    End Sub

    Public Sub ReportCannotConvert(span As TextSpan, fromType As TypeSymbol, toType As TypeSymbol)
      Dim message = $"Cannot convert type '{fromType}' to '{toType}'."
      Report(span, message)
    End Sub

    Public Sub ReportCannotAssign(span As TextSpan, name As String)
      Dim message = $"Variable '{name}' is read-only and cannot be assigned to."
      Report(span, message)
    End Sub

    Public Sub ReportMissingEndIf(span As TextSpan)
      Dim message = $"Missing End If."
      Report(span, message)
    End Sub

    Public Sub ReportMissingIf(span As TextSpan)
      Dim message = $"Missing If."
      Report(span, message)
    End Sub

    Public Sub ReportUnterminatedString(location As TextLocation)
      Dim message = $"Unterminated string literal."
      Report(location.Span, message)
    End Sub

    Public Sub ReportUndefinedFunction(span As TextSpan, name As String)
      Dim message = $"Function '{name}' doesn't exist."
      Report(span, message)
    End Sub

    Public Sub ReportWrongArgumentCount(span As TextSpan, name As String, expectedCount As Integer, actualCount As Integer)
      Dim message = $"Function '{name}' requires {expectedCount} arguments but was given {actualCount}."
      Report(span, message)
    End Sub

    Public Sub ReportWrongArgumentType(span As TextSpan, name As String, expectedType As TypeSymbol, actualType As TypeSymbol)
      Dim message = $"Parameter '{name}' requires a value of type '{expectedType}' but was given a value of type '{actualType}'."
      Report(span, message)
    End Sub

    Public Sub ReportExpressionMustHaveValue(span As TextSpan)
      Dim message = "Expression must have a value."
      Report(span, message)
    End Sub

  End Class

End Namespace
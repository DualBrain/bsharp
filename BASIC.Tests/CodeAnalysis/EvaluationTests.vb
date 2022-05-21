Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports BASIC.CodeAnalysis
Imports BASIC.CodeAnalysis.Syntax

Namespace Global.BASIC.Tests.CodeAnalysis

  Public Class EvaluationTests

    <Theory>
    <InlineData("1", 1)>
    <InlineData("+1", 1)>
    <InlineData("-1", -1)>
    <InlineData("14 + 12", 26)>
    <InlineData("12 - 3", 9)>
    <InlineData("4 * 2", 8)>
    <InlineData("9 / 3", 3)>
    <InlineData("(10)", 10)>
    <InlineData("12 = 3", False)>
    <InlineData("3 = 3", True)>
    <InlineData("12 <> 3", True)>
    <InlineData("3 <> 3", False)>
    <InlineData("false = false", True)>
    <InlineData("true = false", False)>
    <InlineData("false <> false", False)>
    <InlineData("true <> false", True)>
    <InlineData("true", True)>
    <InlineData("false", False)>
    <InlineData("not true", False)>
    <InlineData("not false", True)>
    <InlineData("not false", True)>
    <InlineData("true andalso true", True)>
    <InlineData("false orelse false", False)>
    <InlineData("false or false", False)>
    <InlineData("false or true", True)>
    <InlineData("true or false", True)>
    <InlineData("true or true", True)>
    <InlineData("false and false", False)>
    <InlineData("false and true", False)>
    <InlineData("true and false", False)>
    <InlineData("true and true", True)>
    <InlineData("false xor false", False)>
    <InlineData("false xor true", True)>
    <InlineData("true xor false", True)>
    <InlineData("true xor true", False)>
    <InlineData("1 or 2", 3)>
    <InlineData("1 or 0", 1)>
    <InlineData("1 and 2", 0)>
    <InlineData("1 and 0", 0)>
    <InlineData("1 xor 0", 1)>
    <InlineData("0 xor 1", 1)>
    <InlineData("1 xor 2", 3)>
    <InlineData("dim a = 10", 10)>
    <InlineData("{ dim a = 10
(a * a) }", 100)>
    <InlineData("{ dim a = 0
let a = 10
a * a }", 100)>
    <InlineData("
{ 
  dim a = 0
  if a = 0 then
    let a = 10
  end if
  a
}", 10)>
    <InlineData("
{ 
  dim a = 0
  if a = 4 then
    let a = 10
  end if
  a
}", 0)>
    <InlineData("
{ 
  dim a = 0
  if a = 4 then
    let a = 10
  else
    let a = 20
  end if
  a
}", 20)>
    <InlineData("
{ 
  dim i = 10
  dim result = 0
  while i > 0
  {
    let result = result + i
    let i = i - 1
  }
  wend
  result
}", 55)>
    <InlineData("
{ 
  dim result = 0
  for i = 1 to 10
    let result = result + i
  next
  result
}", 55)>
    Public Sub Evaluator_Computes_CorrectValues(text As String, expectedValue As Object)
      AssertValue(text, expectedValue)
    End Sub

    <Fact>
    Public Sub Evaluator_VariableDeclaration_Reports_Redeclaration()

      Dim text = "
        {
          dim x = 10
          dim y = 100
          {
            dim x = 10
          }
          dim [x] = 5
        }"

      Dim diagnostics = "
        Variable 'x' is already declared."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_BlockStatement_NoInfiniteLoop()

      Dim text = "
        {
         [)]
[]
"

      Dim diagnostics = "
        Unexpected token <CloseParenToken>, expected <IdentifierToken>.
        Unexpected token <EndOfFileToken>, expected <CloseBraceToken>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_NameExpression_Reports_Undefined()

      Dim text = "let [x] * 10"

      Dim diagnostics = "Variable 'x' doesn't exist."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_NameExpression_Reports_NoErrorForInsertedToken()

      Dim text = "1 + []"

      Dim diagnostics = "Unexpected token <EndOfFileToken>, expected <IdentifierToken>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    '<Fact>
    Public Sub Evaluator_UnaryExpression_Reports_Undefined()

      Dim text = "[+]true"

      Dim diagnostics = "Unary operator '+' is not defined for type 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    '<Fact>
    Public Sub Evaluator_BinaryExpression_Reports_Undefined()

      Dim text = "10 [+] false"

      Dim diagnostics = "Binary operator '+' is not defined for type 'int' and 'bool'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_Undefined()

      Dim text = "[x] = 10"

      Dim diagnostics = "Variable 'x' doesn't exist."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_CannotAssign()

      Dim text = "
        {
          const x = 10
          let x [=] 0
        }"

      Dim diagnostics = "Variable 'x' is read-only and cannot be assigned to."

      AssertDiagnostics(text, diagnostics)

    End Sub

    '<Fact>
    Public Sub Evaluator_AssignmentExpression_Reports_CannotConvert()

      Dim text = "
        {
          dim x = 10
          let x [=] [true]
        }"

      Dim diagnostics = "
        Variable 'x' is read-only and cannot be assigned to.
        Cannot convert type 'bool' to 'int'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    Private Shared Sub AssertValue(text As String, expectedValue As Object)
      Dim tree = SyntaxTree.Parse(text)
      Dim c = New Compilation(tree)
      Dim vars = New Dictionary(Of VariableSymbol, Object)
      Dim result = c.Evaluate(vars)
      Assert.Empty(result.Diagnostics)
      Assert.Equal(expectedValue, result.Value)
    End Sub

    Private Shared Sub AssertDiagnostics(text As String, diagnosticText As String)

      Dim at = AnnotatedText.Parse(text)
      Dim tree = SyntaxTree.Parse(at.Text)
      Dim compilation = New Compilation(tree) 'compilation.CreateScript(Nothing, tree)
      Dim result = compilation.Evaluate(New Dictionary(Of VariableSymbol, Object))

      Dim expectedDiagnostics = AnnotatedText.UnindentLines(diagnosticText)

      If at.Spans.Length <> expectedDiagnostics.Length Then
        Throw New Exception("ERROR: Must mark as many spans as there are expected diagnostics.")
      End If

      Assert.Equal(expectedDiagnostics.Length, result.Diagnostics.Length)

      For i = 0 To expectedDiagnostics.Length - 1

        Dim expectedMessage = expectedDiagnostics(i)
        Dim actualMessage = result.Diagnostics(i).Message
        Assert.Equal(expectedMessage, actualMessage)

        Dim expectedSpan = at.Spans(i)
        Dim actualSpan = result.Diagnostics(i).Span 'result.Diagnostics(i).Location.Span
        Assert.Equal(expectedSpan, actualSpan)

      Next

    End Sub

  End Class

End Namespace
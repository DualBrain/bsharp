Option Explicit On
Option Strict On
Option Infer On

Imports Xunit
Imports BASIC.CodeAnalysis
Imports BASIC.CodeAnalysis.Syntax
Imports BASIC.CodeAnalysis.Symbols

Namespace Global.BASIC.Tests.CodeAnalysis

  Public Class EvaluationTests

    <Theory>
    <InlineData("1", 1)>
    <InlineData("not 1", -2)>
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
    <InlineData("""test""", "test")>
    <InlineData("""te""""st""", "te""st")>
    <InlineData("""test""=""test""", True)>
    <InlineData("""test""<>""test""", False)>
    <InlineData("""test""=""abc""", False)>
    <InlineData("""test""<>""abc""", True)>
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
    <InlineData("{ dim b = 0
let b = 10
b * b }", 100)>
    <InlineData("
{ 
  dim c = 0
  if c = 0 then
    let c = 10
  end if
  c
}", 10)>
    <InlineData("
{ 
  dim d = 0
  if d = 4 then
    let d = 10
  end if
  d
}", 0)>
    <InlineData("
{ 
  dim e = 0
  if e = 4 then
    let e = 10
  else
    let e = 20
  end if
  e
}", 20)>
    <InlineData("
{ 
  dim f = 10
  dim result = 0
  while f > 0
  {
    let result = result + f
    let f = f - 1
  }
  wend
  result
}", 55)>
    <InlineData("
{ 
  dim result = 0
  for g = 1 to 10
    let result = result + g
  next
  result
}", 55)>
    <InlineData("
{ 
  dim result = 0
  if true then let result = 10
  result
}", 10)>
    <InlineData("
{ 
  dim result = 0
  if true then let result = 10 else
  result
}", 10)>
    <InlineData("
{ 
  dim result = 0
  if false then let result = 10 else let result = 20
  result
}", 20)>
    <InlineData("
{ 
  dim result = 0
  if true then 
    let result = 10 
  end if
  result
}", 10)>
    <InlineData("
{ 
  dim result = 0
  if true then 
    let result = 10 
  else 
    let result = 20
  end if
  result
}", 10)>
    <InlineData("
{ 
  dim result = 0
  if true then 
    dim h = 0
    let h = h + 10
    let result = h
  else
    dim i = 0
    let i = i + 10
    let result = i
  end if
  let result = result + 10
}", 20)>
    <InlineData("
{ 
  dim j = 10
  for i = 1 to j - 1
    let j = j + i
  next 
  j
}", 55)>
    <InlineData("
{ 
  dim a = 0
  do
    let a = a + 1
  loop while a < 10
  a
}", 10)>
    <InlineData("
if true then
  dim i = 0
  while i < 5
    let i = i + 1
    if i = 5 then
      continue while
    end if
  wend
  i
end if", 5)>
    <InlineData("
if true then
  dim i = 0
  do
    let i = i + 1
    if i = 5 then
      continue do
    end if
  loop while i < 5
  i
end if", 5)>
    Public Sub Evaluator_Computes_CorrectValues(text As String, expectedValue As Object)
      AssertValue(text, expectedValue)
    End Sub
    '    <InlineData("
    '{ 
    '  dim result = 0
    '  if result = 1 then 
    '    dim a = 0
    '    let a = a + 10
    '    let result = a
    '  elseif result = 0 then
    '    dim a = 0
    '    let a = a + 100
    '    let result = a
    '  else
    '    dim b = 0
    '    let b = b + 1
    '    let result = b
    '  end if
    '  let result = result + 10
    '}", 110)>

    <Fact>
    Public Sub Evaluator_DoWhileStatement_Reports_CannotConvert()
      Dim text As String = "
        {
            dim x = 0
            do
              let x = 10
            loop while [10]
        }"

      Dim diagnostics As String = "
          Cannot convert type 'integer' to 'boolean'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_If_MissingEndIf()

      Dim text = "
          if true then 
            dim x = 20 
          else
            dim y = 30
          [][]"

      Dim diagnostics = "
        Unexpected token <EndOfFileToken>, expected <EndKeyword>.
        Unexpected token <EndOfFileToken>, expected <IfKeyword>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_If_MissingEndingIf()

      Dim text = "
          if true then 
            dim x = 20 
          else
            dim y = 30
          [end] [][]"

      Dim diagnostics = "
         Unexpected token <EndKeyword>, expected <IdentifierToken>.
         Unexpected token <EndOfFileToken>, expected <EndKeyword>.
         Unexpected token <EndOfFileToken>, expected <IfKeyword>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_If_MissingTrueStatement()

      Dim text = "
          if true then [][]
"

      Dim diagnostics = "
        Unexpected token <EndOfFileToken>, expected <EndKeyword>.
        Unexpected token <EndOfFileToken>, expected <IfKeyword>."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_If_EmptyTrueStatement()

      Dim text = "
          if true then
          end if
"

      Dim diagnostics = ""

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_If_EmptyTrueAndFalseStatement()

      Dim text = "
          if true then
          else
          end if
"

      Dim diagnostics = ""

      AssertDiagnostics(text, diagnostics)

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
        'x' is already declared."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_Varibles_Can_Shadow_Functions()

      Dim text = "
        {
          dim print = 42
          [print](""test"")
        }"

      Dim diagnostics = "
        Function 'print' doesn't exist."

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

    <Fact>
    Public Sub Evaluator_UnaryExpression_Reports_Undefined()

      Dim text = "[+]true"

      Dim diagnostics = "Unary operator '+' is not defined for type 'boolean'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_BinaryExpression_Reports_Undefined()

      Dim text = "10 [+] false"

      Dim diagnostics = "Binary operator '+' is not defined for type 'integer' and 'boolean'."

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
    Public Shared Sub Evaluator_InvokeFunctionArguments_NoInfiniteLoop()

      Dim text As String = "
        print([""Hi""=][)]"

      Dim diagnostics As String = "
        Unexpected token <CloseParenToken>, expected <IdentifierToken>.
        Parameter 'text' requires a value of type 'string' but was given a value of type '?'."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_FunctionParameters_NoInfiniteLoop()

      Dim text As String = "
        function hi(name as string[[=]][[)]]
            print(""Hi "" + name + ""!"" )
        end function"

      Dim diagnostics As String = "
        Unexpected token <EqualToken>, expected <CloseParenToken>.
        Unexpected token <EqualToken>, expected <IdentifierToken>.
        Unexpected token <CloseParenToken>, expected <IdentifierToken>.
        Unexpected token <CloseParenToken>, expected <IdentifierToken>."

      AssertDiagnostics(text, diagnostics)

    End Sub


    <Fact>
    Public Sub Evaluator_InvokeFunctionArguments_Missing()

      Dim text As String = "
        print([)]"

      Dim diagnostics As String = "
        Function 'print' requires 1 arguments but was given 0."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Sub Evaluator_InvokeFunctionArguments_Exceeding()

      Dim text As String = "
        print(""Hello""[, "" "", "" world!""])"

      Dim diagnostics As String = "
        Function 'print' requires 1 arguments but was given 3."

      AssertDiagnostics(text, diagnostics)

    End Sub

    <Fact>
    Public Shared Sub Evaluator_AssignmentExpression_Reports_CannotConvert()

      Dim text = "
        {
          const x = 10
          let x [=] [true]
        }"

      Dim diagnostics = "
        Variable 'x' is read-only and cannot be assigned to.
        Cannot convert type 'boolean' to 'integer'."

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
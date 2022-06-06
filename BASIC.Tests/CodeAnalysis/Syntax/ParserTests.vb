Option Explicit On
Option Strict On
Option Infer On

Imports Xunit

Namespace Basic.CodeAnalysis.Syntax

  Public Class ParserTests

    <Theory>
    <MemberData(NameOf(GetBinaryOperatorPairsData))>
    Public Sub Parser_BinaryExpression_HonorsPrecedences(op1 As SyntaxKind, op2 As SyntaxKind)
      Dim op1Precedence = SyntaxFacts.GetBinaryOperatorPrecedence(op1)
      Dim op2Precedence = SyntaxFacts.GetBinaryOperatorPrecedence(op2)

      If op1Precedence = 0 Then
        Assert.True(False)
      End If
      If op2Precedence = 0 Then
        Assert.True(False)
      End If

      Dim op1Text = SyntaxFacts.GetText(op1)
      Dim op2Text = SyntaxFacts.GetText(op2)
      If op1Text Is Nothing Then
        Assert.True(False)
      End If
      If op2Text Is Nothing Then
        Assert.True(False)
      End If
      Dim text = $"a {op1Text} b {op2Text} c"
      Dim expression = ParseExpression(text)

      If op1Precedence >= op2Precedence Then
        '    op2
        '    / \
        '  op1  c
        '  / \
        ' a   b
        Using e = New AssertingEnumerator(expression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "a")
          e.AssertToken(op1, op1Text)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "b")
          e.AssertToken(op2, op2Text)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "c")
        End Using
      Else
        '  op1
        '  / \
        ' a  op2
        '    / \
        '   b   c
        Using e = New AssertingEnumerator(expression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "a")
          e.AssertToken(op1, op1Text)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "b")
          e.AssertToken(op2, op2Text)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "c")
        End Using
      End If
    End Sub

    <Theory>
    <MemberData(NameOf(GetUnaryOperatorPairsData))>
    Public Sub Parser_UnaryExpression_HonorsPrecedences(unaryKind As SyntaxKind, binaryKind As SyntaxKind)

      Dim unaryPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(unaryKind)
      Dim binaryPrecedence = SyntaxFacts.GetBinaryOperatorPrecedence(binaryKind)
      Dim unaryText = SyntaxFacts.GetText(unaryKind)
      Dim binaryText = SyntaxFacts.GetText(binaryKind)
      Dim text = $"{unaryText} a {binaryText} b"
      Dim expression = ParseExpression(text)

      If unaryPrecedence >= binaryPrecedence Then

        '  binary
        '   /  \
        'unary  b
        '  |
        '  a

        Using e = New AssertingEnumerator(expression)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.UnaryExpression)
          e.AssertToken(unaryKind, unaryText)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "a")
          e.AssertToken(binaryKind, binaryText)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "b")
        End Using

      Else

        '  unary
        '    |
        '  binary
        '   / \
        '  a   b

        Using e = New AssertingEnumerator(expression)
          e.AssertNode(SyntaxKind.UnaryExpression)
          e.AssertToken(unaryKind, unaryText)
          e.AssertNode(SyntaxKind.BinaryExpression)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "a")
          e.AssertToken(binaryKind, binaryText)
          e.AssertNode(SyntaxKind.NameExpression)
          e.AssertToken(SyntaxKind.IdentifierToken, "b")
        End Using

      End If

    End Sub

    Private Shared Function ParseExpression(text As String) As ExpressionSyntax
      Dim tree = SyntaxTree.Parse(text)
      Dim root = tree.Root
      'Dim statement = root.Statement
      'Return Assert.IsType(Of ExpressionStatementSyntax)(statement).Expression
      Dim member = Assert.Single(root.Members)
      Dim globalStatement = Assert.IsType(Of GlobalStatementSyntax)(member)
      Return Assert.IsType(Of ExpressionStatementSyntax)(globalStatement.Statement).Expression
    End Function

    Public Shared Iterator Function GetBinaryOperatorPairsData() As IEnumerable(Of Object())
      For Each op1 In SyntaxFacts.GetBinaryOperatorKinds
        For Each op2 In SyntaxFacts.GetBinaryOperatorKinds
          Yield New Object() {op1, op2}
        Next
      Next
    End Function

    Public Shared Iterator Function GetUnaryOperatorPairsData() As IEnumerable(Of Object())
      For Each unary In SyntaxFacts.GetUnaryOperatorKinds
        For Each binary In SyntaxFacts.GetBinaryOperatorKinds
          Yield New Object() {unary, binary}
        Next
      Next
    End Function

  End Class

End Namespace
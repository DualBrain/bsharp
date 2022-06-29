Imports System.CodeDom.Compiler
Imports System.IO
Imports System.Runtime.CompilerServices
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax
Imports Bsharp.IO

Namespace Bsharp.CodeAnalysis.Binding

  Friend Module BoundNodePrinter

    <Extension>
    Public Sub WriteTo(node As BoundNode, writer As TextWriter)
      If TypeOf writer Is IndentedTextWriter Then
        WriteTo(node, DirectCast(writer, IndentedTextWriter))
      Else
        WriteTo(node, New IndentedTextWriter(writer))
      End If
    End Sub

    <Extension>
    Public Sub WriteTo(node As BoundNode, writer As IndentedTextWriter)
      Select Case node.Kind
        Case BoundNodeKind.AssignmentExpression : WriteAssignmentExpression(CType(node, BoundAssignmentExpression), writer)
        Case BoundNodeKind.BinaryExpression : WriteBinaryExpression(CType(node, BoundBinaryExpression), writer)
        Case BoundNodeKind.BlockStatement : WriteBlockStatement(CType(node, BoundBlockStatement), writer)
        Case BoundNodeKind.CallExpression : WriteCallExpression(CType(node, BoundCallExpression), writer)
        Case BoundNodeKind.ConditionalGotoStatement : WriteConditionalGotoStatement(CType(node, BoundConditionalGotoStatement), writer)
        Case BoundNodeKind.ConversionExpression : WriteConversionExpression(CType(node, BoundConversionExpression), writer)
        Case BoundNodeKind.DoWhileStatement : WriteDoWhileStatement(CType(node, BoundDoWhileStatement), writer)
        Case BoundNodeKind.ErrorExpression : WriteErrorExpression(CType(node, BoundErrorExpression), writer)
        Case BoundNodeKind.ExpressionStatement : WriteExpressionStatement(CType(node, BoundExpressionStatement), writer)
        Case BoundNodeKind.ForStatement : WriteForStatement(CType(node, BoundForStatement), writer)
        Case BoundNodeKind.GotoStatement : WriteGotoStatement(CType(node, BoundGotoStatement), writer)
        Case BoundNodeKind.IfStatement : WriteIfStatement(CType(node, BoundIfStatement), writer)
        Case BoundNodeKind.LabelStatement : WriteLabelStatement(CType(node, BoundLabelStatement), writer)
        Case BoundNodeKind.LiteralExpression : WriteLiteralExpression(CType(node, BoundLiteralExpression), writer)
        Case BoundNodeKind.NopStatement : WriteNopStatement(CType(node, BoundNopStatement), writer)
        Case BoundNodeKind.PrintStatement : WritePrintStatement(CType(node, BoundPrintStatement), writer)
        Case BoundNodeKind.ReturnStatement : WriteReturnStatement(CType(node, BoundReturnStatement), writer)
        Case BoundNodeKind.UnaryExpression : WriteUnaryExpression(CType(node, BoundUnaryExpression), writer)
        Case BoundNodeKind.VariableDeclaration : WriteVariableDeclaration(CType(node, BoundVariableDeclaration), writer)
        Case BoundNodeKind.VariableExpression : WriteVariableExpression(CType(node, BoundVariableExpression), writer)
        Case BoundNodeKind.WhileStatement : WriteWhileStatement(CType(node, BoundWhileStatement), writer)
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select
    End Sub

    <Extension>
    Private Sub WriteNestedStatement(writer As IndentedTextWriter, node As BoundStatement)

      Dim needsIndentation = TypeOf node IsNot BoundBlockStatement

      If needsIndentation Then
        writer.Indent += 1
      End If

      node.WriteTo(writer)

      If needsIndentation Then
        writer.Indent -= 1
      End If

    End Sub

    <Extension>
    Private Sub WriteNestedExpression(writer As IndentedTextWriter, parentPrecedence As Integer, expression As BoundExpression)
      If TypeOf expression Is BoundUnaryExpression Then
        Dim unary = CType(expression, BoundUnaryExpression)
        writer.WriteNestedExpression(parentPrecedence, SyntaxFacts.GetUnaryOperatorPrecedence(unary.Op.SyntaxKind), unary)
      ElseIf TypeOf expression Is BoundBinaryExpression Then
        Dim binary = CType(expression, BoundBinaryExpression)
        writer.WriteNestedExpression(parentPrecedence, SyntaxFacts.GetBinaryOperatorPrecedence(binary.Op.SyntaxKind), binary)
      Else
        expression.WriteTo(writer)
      End If
    End Sub

    <Extension>
    Private Sub WriteNestedExpression(writer As IndentedTextWriter, parentPrecedence As Integer, currentPrecedence As Integer, expression As BoundExpression)

      Dim needsParenthesis = parentPrecedence >= currentPrecedence

      If needsParenthesis Then
        writer.WritePunctuation(SyntaxKind.OpenParenToken)
      End If

      expression.WriteTo(writer)

      If needsParenthesis Then
        writer.WritePunctuation(SyntaxKind.CloseParenToken)
      End If

    End Sub

    Private Sub WriteBlockStatement(node As BoundBlockStatement, writer As IndentedTextWriter)

      writer.WritePunctuation(SyntaxKind.OpenBraceToken)
      writer.WriteLine()
      writer.Indent += 1

      For Each s In node.Statements
        s.WriteTo(writer)
      Next

      writer.Indent -= 1
      writer.WritePunctuation(SyntaxKind.CloseBraceToken)
      writer.WriteLine()

    End Sub

    Private Sub WritePrintStatement(node As BoundPrintStatement, writer As IndentedTextWriter)
      writer.WriteKeyword(SyntaxKind.PrintKeyword)
      writer.WriteSpace
      For Each nd In node.Nodes
        nd.WriteTo(writer)
      Next
      writer.WriteLine()
    End Sub

    Private Sub WriteNopStatement(node As BoundNopStatement, writer As IndentedTextWriter)
      If node Is Nothing Then
      End If
      writer.WriteKeyword("nop")
      writer.WriteLine()
    End Sub

    Private Sub WriteVariableDeclaration(node As BoundVariableDeclaration, writer As IndentedTextWriter)
      writer.WriteKeyword(If(node.Variable.IsReadOnly, SyntaxKind.ConstKeyword, SyntaxKind.DimKeyword))
      writer.WriteSpace
      writer.WriteIdentifier(node.Variable.Name)
      writer.WriteSpace
      writer.WritePunctuation(SyntaxKind.EqualToken)
      writer.WriteSpace
      node.Initializer.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteIfStatement(node As BoundIfStatement, writer As IndentedTextWriter)
      writer.WriteKeyword(SyntaxKind.IfKeyword)
      writer.WriteSpace
      node.Expression.WriteTo(writer)
      writer.WriteLine()
      writer.WriteNestedStatement(node.Statements)
      If node.ElseStatement IsNot Nothing Then
        writer.WriteKeyword(SyntaxKind.ElseKeyword)
        writer.WriteLine()
        writer.WriteNestedStatement(node.ElseStatement)
      End If
    End Sub

    Private Sub WriteWhileStatement(node As BoundWhileStatement, writer As IndentedTextWriter)
      writer.WriteKeyword(SyntaxKind.WhileKeyword)
      writer.WriteSpace
      node.Expression.WriteTo(writer)
      writer.WriteLine()
      writer.WriteNestedStatement(node.Statements)
    End Sub

    Private Sub WriteDoWhileStatement(node As BoundDoWhileStatement, writer As IndentedTextWriter)
      writer.WriteKeyword(SyntaxKind.DoKeyword)
      writer.WriteLine()
      writer.WriteNestedStatement(node.Statements)
      writer.WriteKeyword(SyntaxKind.WhileKeyword)
      writer.WriteSpace
      node.Expression.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteForStatement(node As BoundForStatement, writer As IndentedTextWriter)
      writer.WriteKeyword(SyntaxKind.ForKeyword)
      writer.WriteSpace
      writer.WriteIdentifier(node.Variable.Name)
      writer.WriteSpace
      writer.WritePunctuation(SyntaxKind.EqualToken)
      writer.WriteSpace
      node.LowerBound.WriteTo(writer)
      writer.WriteSpace
      writer.WriteKeyword(SyntaxKind.ToKeyword)
      writer.WriteSpace
      node.UpperBound.WriteTo(writer)
      writer.WriteLine()
      writer.WriteNestedStatement(node.Body)
    End Sub

    Private Sub WriteLabelStatement(node As BoundLabelStatement, writer As IndentedTextWriter)

      Dim unindent = writer.Indent > 0

      If unindent Then
        writer.Indent -= 1
      End If

      writer.WritePunctuation(node.Label.Name)
      writer.WritePunctuation(SyntaxKind.ColonToken)
      writer.WriteLine()

      If unindent Then
        writer.Indent += 1
      End If

    End Sub

    Private Sub WriteGotoStatement(node As BoundGotoStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("goto") ' There is no SyntaxKind for goto
      writer.WriteSpace()
      writer.WriteIdentifier(node.Label.Name)
      writer.WriteLine()
    End Sub

    Private Sub WriteConditionalGotoStatement(node As BoundConditionalGotoStatement, writer As IndentedTextWriter)
      writer.WriteKeyword("goto") ' There is no SyntaxKind for goto
      writer.WriteSpace()
      writer.WriteIdentifier(node.Label.Name)
      writer.WriteSpace()
      writer.WriteKeyword(If(node.JumpIfTrue, "if", "unless"))
      writer.WriteSpace()
      node.Condition.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteReturnStatement(node As BoundReturnStatement, writer As IndentedTextWriter)
      writer.WriteKeyword(SyntaxKind.ReturnKeyword)
      If node.Expression IsNot Nothing Then
        writer.WriteSpace()
        node.Expression.WriteTo(writer)
      End If
      writer.WriteLine()
    End Sub

    Private Sub WriteExpressionStatement(node As BoundExpressionStatement, writer As IndentedTextWriter)
      node.Expression.WriteTo(writer)
      writer.WriteLine()
    End Sub

    Private Sub WriteErrorExpression(node As BoundErrorExpression, writer As IndentedTextWriter)
      If node Is Nothing Then
      End If
      writer.WriteKeyword("?")
    End Sub

    Private Sub WriteLiteralExpression(node As BoundLiteralExpression, writer As IndentedTextWriter)
      Dim value = node.Value.ToString()
      If node.Type Is TypeSymbol.Boolean Then
        writer.WriteKeyword(If(CBool(value), SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword))
      ElseIf node.Type Is TypeSymbol.Decimal OrElse
             node.Type Is TypeSymbol.Double OrElse
             node.Type Is TypeSymbol.Single OrElse
             node.Type Is TypeSymbol.ULong64 OrElse
             node.Type Is TypeSymbol.Long64 OrElse
             node.Type Is TypeSymbol.ULong OrElse
             node.Type Is TypeSymbol.Long OrElse
             node.Type Is TypeSymbol.UInteger OrElse
             node.Type Is TypeSymbol.Integer OrElse
             node.Type Is TypeSymbol.SByte OrElse
             node.Type Is TypeSymbol.Byte Then
        writer.WriteNumber(value)
      ElseIf node.Type Is TypeSymbol.Char Then
        'TODO: "A"c
        value = """" & value.Replace("""", """""") & """"
        writer.WriteString(value)
      ElseIf node.Type Is TypeSymbol.String Then
        value = """" & value.Replace("""", """""") & """"
        writer.WriteString(value)
      Else
        Throw New Exception($"Unexpected type {node.Type}")
      End If
    End Sub

    Private Sub WriteVariableExpression(node As BoundVariableExpression, writer As IndentedTextWriter)
      writer.WriteIdentifier(node.Variable.Name)
    End Sub

    Private Sub WriteAssignmentExpression(node As BoundAssignmentExpression, writer As IndentedTextWriter)
      writer.WriteIdentifier(node.Variable.Name)
      writer.WriteSpace
      writer.WritePunctuation(SyntaxKind.EqualToken)
      writer.WriteSpace
      node.Expression.WriteTo(writer)
    End Sub

    Private Sub WriteUnaryExpression(node As BoundUnaryExpression, writer As IndentedTextWriter)
      Dim precedence = SyntaxFacts.GetUnaryOperatorPrecedence(node.Op.SyntaxKind)
      writer.WritePunctuation(node.Op.SyntaxKind)
      writer.WriteNestedExpression(precedence, node.Operand)
    End Sub

    Private Sub WriteBinaryExpression(node As BoundBinaryExpression, writer As IndentedTextWriter)
      Dim precedence = SyntaxFacts.GetBinaryOperatorPrecedence(node.Op.SyntaxKind)
      writer.WriteNestedExpression(precedence, node.Left)
      writer.WriteSpace
      writer.WritePunctuation(node.Op.SyntaxKind)
      writer.WriteSpace
      writer.WriteNestedExpression(precedence, node.Right)
    End Sub

    Private Sub WriteCallExpression(node As BoundCallExpression, writer As IndentedTextWriter)

      writer.WriteIdentifier(node.Function.Name)
      writer.WritePunctuation(SyntaxKind.OpenParenToken)

      Dim isFirst = True

      For Each argument In node.Arguments

        If isFirst Then
          isFirst = False
        Else
          writer.WritePunctuation(SyntaxKind.CommaToken)
          writer.WriteSpace
        End If

        argument.WriteTo(writer)

      Next

      writer.WritePunctuation(SyntaxKind.CloseParenToken)

    End Sub

    Private Sub WriteConversionExpression(node As BoundConversionExpression, writer As IndentedTextWriter)
      writer.WriteIdentifier(node.Type.Name)
      writer.WritePunctuation(SyntaxKind.OpenParenToken)
      node.Expression.WriteTo(writer)
      writer.WritePunctuation(SyntaxKind.CloseParenToken)
    End Sub

  End Module

End Namespace
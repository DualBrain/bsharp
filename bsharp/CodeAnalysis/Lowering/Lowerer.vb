'Imports System.Collections.Immutable
Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Binding
Imports Bsharp.CodeAnalysis.Symbols
'Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis.Lowering

  ' TODO: Consider creating a BoundNodeFactory to construct nodes to make lowering easier to read.
  Friend NotInheritable Class Lowerer
    Inherits BoundTreeRewriter

    Private m_labelCount As Integer = 0

    Private Sub New()
    End Sub

    Private Function GenerateLabel() As BoundLabel
      m_labelCount += 1
      Dim name = $"Label{m_labelCount}"
      Return New BoundLabel(name)
    End Function

    Public Shared Function Lower(func As FunctionSymbol, statement As BoundStatement) As BoundBlockStatement
      Dim lowerer = New Lowerer
      Dim result = lowerer.RewriteStatement(statement)
      Return RemoveDeadCode(Flatten(func, result))
    End Function

    Private Shared Function Flatten(func As FunctionSymbol, statement As BoundStatement) As BoundBlockStatement
      Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)
      Dim stack = New Stack(Of BoundStatement)
      stack.Push(statement)
      While stack.Count > 0
        Dim current = stack.Pop
        If TypeOf current Is BoundBlockStatement Then
          For Each s In DirectCast(current, BoundBlockStatement).Statements.Reverse()
            stack.Push(s)
          Next
        Else
          builder.Add(current)
        End If
      End While
      If func.Type Is TypeSymbol.Nothing Then
        If builder.Count = 0 OrElse CanFallThrough(builder.Last) Then
          builder.Add(New BoundReturnStatement(Nothing))
        End If
      End If
      Return New BoundBlockStatement(builder.ToImmutable)
    End Function

    Private Shared Function CanFallThrough(boundStatement As BoundStatement) As Boolean
      Return boundStatement.Kind <> BoundNodeKind.ReturnStatement AndAlso
             boundStatement.Kind <> BoundNodeKind.GotoStatement
    End Function

    Private Shared Function RemoveDeadCode(node As BoundBlockStatement) As BoundBlockStatement

      Dim controlFlow = ControlFlowGraph.Create(node)
      Dim reachableStatements = New HashSet(Of BoundStatement)(controlFlow.Blocks.SelectMany(Function(b) b.Statements))

      Dim builder = node.Statements.ToBuilder
      For i = builder.Count - 1 To 0 Step -1
        If Not reachableStatements.Contains(builder(i)) Then
          builder.RemoveAt(i)
        End If
      Next

      Return New BoundBlockStatement(builder.ToImmutable)

    End Function

    Protected Overrides Function RewriteIfStatement(node As BoundIfStatement) As BoundStatement

      If node.ElseStatement Is Nothing Then

        ' IF *expression* THEN
        '   *statements*
        ' END IF
        '
        ' ------>
        '
        ' gotoFalse *expression* end
        '   *statements*
        ' end:

        Dim endLabel = GenerateLabel()
        Dim gotoFalse = New BoundConditionalGotoStatement(endLabel, node.Expression, False)
        Dim endLabelStatement = New BoundLabelStatement(endLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          gotoFalse,
          node.Statements,
          endLabelStatement))

        Return RewriteStatement(result)

      Else

        ' if <condition> then
        '   <trueBody>
        ' else
        '   <falseBody>
        ' end if
        '
        ' ------>
        '
        ' gotoFalse <condition> else
        ' <trueBody>
        ' goto end
        ' else:
        ' <falseBody>
        ' end:

        Dim elseLabel = GenerateLabel()
        Dim endLabel = GenerateLabel()

        Dim gotoFalse = New BoundConditionalGotoStatement(elseLabel, node.Expression, False)
        Dim gotoEndStatement = New BoundGotoStatement(endLabel)

        Dim elseLabelStatement = New BoundLabelStatement(elseLabel)
        Dim endLabelStatement = New BoundLabelStatement(endLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          gotoFalse,
          node.Statements,
          gotoEndStatement,
          elseLabelStatement,
          node.ElseStatement,
          endLabelStatement))

        Return RewriteStatement(result)

      End If

    End Function

    Protected Overrides Function RewriteWhileStatement(node As BoundWhileStatement) As BoundStatement

      ' WHILE *expression*
      '   *statements*
      ' wend
      '
      ' ------->
      '
      ' goto continue
      ' body:
      '   *statements*
      ' continue:
      '   gotoTrue *expression* body
      ' exit:

      Dim bodyLabel = GenerateLabel()

      Dim gotoContinue = New BoundGotoStatement(node.ContinueLabel)
      Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
      Dim gotoTrue = New BoundConditionalGotoStatement(bodyLabel, node.Expression)
      Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
        gotoContinue,
        bodyLabelStatement,
        node.Statements,
        continueLabelStatement,
        gotoTrue,
        exitLabelStatement))

      Return RewriteStatement(result)

    End Function

    Protected Overrides Function RewriteDoUntilStatement(node As BoundDoUntilStatement) As BoundStatement

      ' do [until *expression*]
      '   *statements*
      ' loop [until *expression*]
      '
      ' ------->
      '
      ' body:
      '   *statements*
      ' continue:
      ' gotoTrue *expression* body
      ' exit:

      Dim bodyLabel = GenerateLabel()
      Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
      Dim gotoTrue = New BoundConditionalGotoStatement(bodyLabel, node.Expression)
      Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
        bodyLabelStatement,
        node.Statements,
        continueLabelStatement,
        gotoTrue,
        exitLabelStatement))

      Return RewriteStatement(result)

    End Function

    Protected Overrides Function RewriteDoWhileStatement(node As BoundDoWhileStatement) As BoundStatement

      ' do [while *expression*]
      '   *statements*
      ' loop [while *expression*]

      ' do [until *expression*]
      '   *statements*
      ' loop [until *expression*]
      '
      ' ------->
      '
      ' body:
      '   *statements*
      ' check:
      ' continue:
      ' gotoTrue *expression* body
      ' break:

      Dim bodyLabel = GenerateLabel()
      Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
      Dim gotoTrue = New BoundConditionalGotoStatement(bodyLabel, node.Expression)
      Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
        bodyLabelStatement,
        node.Statements,
        continueLabelStatement,
        gotoTrue,
        exitLabelStatement))

      Return RewriteStatement(result)

    End Function

    Protected Overrides Function RewriteForStatement(node As BoundForStatement) As BoundStatement

      ' for <var> = <lower> to <upper> step <stepper>
      '   <body>
      ' next
      '
      '  ------>
      ' 
      ' dim <var> = <lower>
      ' while <var> <= <upper>
      '   <body>
      '   <var> = <var> + 1
      ' wend
      '

      Dim variableDeclaration = New BoundVariableDeclaration(node.Variable, node.LowerBound)

      Dim variableExpression = New BoundVariableExpression(node.Variable)

      Dim upperBoundSymbol = New LocalVariableSymbol("upperBound", True, TypeSymbol.Integer, node.UpperBound.ConstantValue)
      Dim upperBoundDeclaration = New BoundVariableDeclaration(upperBoundSymbol, node.UpperBound)

      Dim condition = New BoundBinaryExpression(
              variableExpression,
              BoundBinaryOperator.Bind(SyntaxKind.LessThanEqualToken, TypeSymbol.Integer, TypeSymbol.Integer),
              New BoundVariableExpression(upperBoundSymbol))

      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)

      Dim increment = New BoundExpressionStatement(
              New BoundAssignmentExpression(
                node.Variable,
                New BoundBinaryExpression(
                  variableExpression,
                  BoundBinaryOperator.Bind(SyntaxKind.PlusToken, TypeSymbol.Integer, TypeSymbol.Integer),
                  New BoundLiteralExpression(1))))

      Dim whileBody = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          node.Body,
          continueLabelStatement,
          increment))
      Dim whileStatement = New BoundWhileStatement(condition, whileBody, node.ExitLabel, GenerateLabel)
      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
              variableDeclaration,
              upperBoundDeclaration,
              whileStatement))

      Return RewriteStatement(result)

    End Function

    Protected Overrides Function RewriteConditionalGotoStatement(node As BoundConditionalGotoStatement) As BoundStatement

      If node.Condition.ConstantValue IsNot Nothing Then
        Dim condition = CBool(node.Condition.ConstantValue.Value)
        condition = If(node.JumpIfTrue, condition, Not condition)
        If condition Then
          Return RewriteStatement(New BoundGotoStatement(node.Label))
        Else
          Return RewriteStatement(New BoundNopStatement())
        End If
      End If

      Return MyBase.RewriteConditionalGotoStatement(node)

    End Function

  End Class

End Namespace
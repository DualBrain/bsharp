'Imports System.Collections.Immutable
Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Binding
'Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis.Lowering

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

    Public Shared Function Lower(statement As BoundStatement) As BoundBlockStatement
      Dim lowerer = New Lowerer
      Dim result = lowerer.RewriteStatement(statement)
      Return Flatten(result)
    End Function

    Private Shared Function Flatten(statement As BoundStatement) As BoundBlockStatement
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
      Return New BoundBlockStatement(builder.ToImmutable)
    End Function

    '    Private Shared Function CanFallThrough(boundStatement As BoundStatement) As Boolean
    '      Return boundStatement.Kind <> BoundNodeKind.ReturnStatement AndAlso
    '             boundStatement.Kind <> BoundNodeKind.GotoStatement
    '    End Function

    '    Private Shared Function RemoveDeadCode(node As BoundBlockStatement) As BoundBlockStatement

    '      Dim controlFlow = ControlFlowGraph.Create(node)
    '      Dim reachableStatements = New HashSet(Of BoundStatement)(controlFlow.Blocks.SelectMany(Function(b) b.Statements))

    '      Dim builder = node.Statements.ToBuilder
    '      For i = builder.Count - 1 To 0 Step -1
    '        If Not reachableStatements.Contains(builder(i)) Then
    '          builder.RemoveAt(i)
    '        End If
    '      Next

    '      Return New BoundBlockStatement(builder.ToImmutable)

    '    End Function

    Protected Overrides Function RewriteIfStatement(node As BoundIfStatement) As BoundStatement

      If Not node.ElseStatements.Any Then

        ' if <condition> then
        '   <trueBody>
        ' end if
        '
        ' ------>
        '
        ' gotoFalse <condition> end
        ' <trueBody>
        ' end:

        Dim endLabel = GenerateLabel()
        Dim gotoFalse = New BoundConditionalGotoStatement(endLabel, node.Condition, False)
        Dim endLabelStatement = New BoundLabelStatement(endLabel)
        Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)
        builder.Add(gotoFalse)
        For Each statement In node.IfStatements
          builder.Add(statement)
        Next
        builder.Add(endLabelStatement)
        Dim result = New BoundBlockStatement(builder.ToImmutable) 'ImmutableArray.Create(Of BoundStatement)(gotoFalse, node.ThenStatement, endLabelStatement))
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

        Dim gotoFalse = New BoundConditionalGotoStatement(elseLabel, node.Condition, False)
        Dim gotoEndStatement = New BoundGotoStatement(endLabel)

        Dim elseLabelStatement = New BoundLabelStatement(elseLabel)
        Dim endLabelStatement = New BoundLabelStatement(endLabel)

        Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)
        builder.Add(gotoFalse)
        For Each statement In node.IfStatements
          builder.Add(statement)
        Next
        builder.Add(gotoEndStatement)
        builder.Add(elseLabelStatement)
        For Each statement In node.ElseStatements
          builder.Add(statement)
        Next
        builder.Add(endLabelStatement)
        Dim result = New BoundBlockStatement(builder.ToImmutable)

        'Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(gotoFalse,
        '                                                                              node.ThenStatement,
        '                                                                              gotoEndStatement,
        '                                                                              elseLabelStatement,
        '                                                                              node.ElseStatement,
        '                                                                              endLabelStatement))
        Return RewriteStatement(result)

      End If

    End Function

    Protected Overrides Function RewriteWhileStatement(node As BoundWhileStatement) As BoundStatement

      ' while <condition>
      '   <body>
      ' wend
      '
      ' ------->
      '
      ' goto continue
      ' body:
      ' <body>
      ' continue:
      ' gotoTrue <condition> body
      ' break:

      Dim continueLabel = GenerateLabel()
      Dim checkLabel = GenerateLabel()
      Dim endLabel = GenerateLabel()

      Dim gotoCheck = New BoundGotoStatement(checkLabel)
      Dim continueLabelStatement = New BoundLabelStatement(continueLabel)
      Dim checkLabelStatement = New BoundLabelStatement(checkLabel)
      Dim gotoTrue = New BoundConditionalGotoStatement(continueLabel, node.Condition, True)
      Dim endLabelStatement = New BoundLabelStatement(endLabel)

      Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)
      builder.Add(gotoCheck)
      builder.Add(continueLabelStatement)
      'For Each statement In node.Body
      builder.Add(node.Body)
      'Next
      builder.Add(checkLabelStatement)
      builder.Add(gotoTrue)
      builder.Add(endLabelStatement)
      Dim result = New BoundBlockStatement(builder.ToImmutable)

      ' ------

      'Dim bodyLabel = GenerateLabel()

      'Dim gotoContinue = New BoundGotoStatement(node.ContinueLabel)
      'Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
      'Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
      'Dim gotoTrue = New BoundConditionalGotoStatement(bodyLabel, node.Condition)
      'Dim breakLabelStatement = New BoundLabelStatement(node.BreakLabel)

      'Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
      '  gotoContinue,
      '  bodyLabelStatement,
      '  node.Body,
      '  continueLabelStatement,
      '  gotoTrue,
      '  breakLabelStatement))

      Return RewriteStatement(result)

    End Function

    '    Protected Overrides Function RewriteDoWhileStatement(node As BoundDoWhileStatement) As BoundStatement

    '      ' do 
    '      '   <body>
    '      ' while <condition>
    '      '
    '      ' ------->
    '      '
    '      ' body:
    '      ' <body>
    '      ' check:
    '      ' continue:
    '      ' gotoTrue <condition> body
    '      ' break:

    '      Dim bodyLabel = GenerateLabel()

    '      Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
    '      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
    '      Dim gotoTrue = New BoundConditionalGotoStatement(bodyLabel, node.Condition)
    '      Dim breakLabelStatement = New BoundLabelStatement(node.BreakLabel)

    '      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(bodyLabelStatement,
    '                                                                                    node.Body,
    '                                                                                    continueLabelStatement,
    '                                                                                    gotoTrue,
    '                                                                                    breakLabelStatement))

    '      Return RewriteStatement(result)

    '    End Function

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

      Dim condition = New BoundBinaryExpression(
              variableExpression,
              BoundBinaryOperator.Bind(SyntaxKind.LessThanEqualToken, GetType(Integer), GetType(Integer)),
              node.UpperBound)

      Dim increment = New BoundExpressionStatement(
              New BoundAssignmentExpression(
                node.Variable,
                New BoundBinaryExpression(
                  variableExpression,
                  BoundBinaryOperator.Bind(SyntaxKind.PlusToken, GetType(Integer), GetType(Integer)),
                  New BoundLiteralExpression(1))))

      Dim whileBody = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(node.Body, increment))
      Dim whileStatement = New BoundWhileStatement(condition, whileBody)
      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
              variableDeclaration,
              whileStatement))

      Return RewriteStatement(result)

    End Function

    '    Protected Overrides Function RewriteConditionalGotoStatement(node As BoundConditionalGotoStatement) As BoundStatement

    '      If node.Condition.ConstantValue IsNot Nothing Then
    '        Dim condition = CBool(node.Condition.ConstantValue.Value)
    '        condition = If(node.JumpIfTrue, condition, Not condition)
    '        If condition Then
    '          Return RewriteStatement(New BoundGotoStatement(node.Label))
    '        Else
    '          Return RewriteStatement(New BoundNopStatement())
    '        End If
    '      End If

    '      Return MyBase.RewriteConditionalGotoStatement(node)

    '    End Function

  End Class

End Namespace
Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundTreeRewriter

    Public Overridable Function RewriteStatement(node As BoundStatement) As BoundStatement
      Select Case node.Kind
        Case BoundNodeKind.BlockStatement : Return RewriteBlockStatement(DirectCast(node, BoundBlockStatement))
        'Case BoundNodeKind.NopStatement : Return RewriteNopStatement(DirectCast(node, BoundNopStatement))
        Case BoundNodeKind.VariableDeclaration : Return RewriteVariableDeclaration(DirectCast(node, BoundVariableDeclaration))
        Case BoundNodeKind.IfStatement : Return RewriteIfStatement(DirectCast(node, BoundIfStatement))
        Case BoundNodeKind.WhileStatement : Return RewriteWhileStatement(DirectCast(node, BoundWhileStatement))
        'Case BoundNodeKind.DoWhileStatement : Return RewriteDoWhileStatement(DirectCast(node, BoundDoWhileStatement))
        Case BoundNodeKind.ForStatement : Return RewriteForStatement(DirectCast(node, BoundForStatement))
        Case BoundNodeKind.LabelStatement : Return RewriteLabeltatement(DirectCast(node, BoundLabelStatement))
        Case BoundNodeKind.GotoStatement : Return RewriteGotoStatement(DirectCast(node, BoundGotoStatement))
        Case BoundNodeKind.ConditionalGotoStatement : Return RewriteConditionalGotoStatement(DirectCast(node, BoundConditionalGotoStatement))
        'Case BoundNodeKind.ReturnStatement : Return RewriteReturnStatement(DirectCast(node, BoundReturnStatement))
        Case BoundNodeKind.ExpressionStatement : Return RewriteExpressionStatement(DirectCast(node, BoundExpressionStatement))
        Case Else
          Throw New Exception($"Unexpected node: {node.Kind}")
      End Select
    End Function

    Public Overridable Function RewriteExpression(node As BoundExpression) As BoundExpression
      If node Is Nothing Then Return node
      Select Case node.Kind
        Case BoundNodeKind.ErrorExpression : Return RewriteErrorExpression(DirectCast(node, BoundErrorExpression))
        Case BoundNodeKind.LiteralExpression : Return RewriteLiteralExpression(DirectCast(node, BoundLiteralExpression))
        Case BoundNodeKind.VariableExpression : Return RewriteVariableExpression(DirectCast(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : Return RewriteAssignmentExpression(DirectCast(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : Return RewriteUnaryExpression(DirectCast(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : Return RewriteBinaryExpression(DirectCast(node, BoundBinaryExpression))
          'Case BoundNodeKind.CallExpression : Return RewriteCallExpression(DirectCast(node, BoundCallExpression))
          'Case BoundNodeKind.ConversionExpression : Return RewriteConversionExpression(DirectCast(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node: {node.Kind}")
      End Select
    End Function

    Protected Overridable Function RewriteStatements(statements As ImmutableArray(Of BoundStatement)) As ImmutableArray(Of BoundStatement)
      Dim builder As ImmutableArray(Of BoundStatement).Builder = Nothing
      For i = 0 To statements.Length - 1
        Dim oldStatement = statements(i)
        Dim newStatement = RewriteStatement(oldStatement)
        If newStatement IsNot oldStatement Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of BoundStatement)(statements.Length)
            For j = 0 To i - 1
              builder.Add(statements(j))
            Next
          End If
        End If
        If builder IsNot Nothing Then
          builder.Add(newStatement)
        End If
      Next
      If builder Is Nothing Then
        Return statements
      End If
      Return builder.MoveToImmutable
    End Function

    Protected Overridable Function RewriteBlockStatement(node As BoundBlockStatement) As BoundStatement
      Dim builder As ImmutableArray(Of BoundStatement).Builder = Nothing
      For i = 0 To node.Statements.Length - 1
        Dim oldStatement = node.Statements(i)
        Dim newStatement = RewriteStatement(oldStatement)
        If newStatement IsNot oldStatement Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of BoundStatement)(node.Statements.Length)
            For j = 0 To i - 1
              builder.Add(node.Statements(j))
            Next
          End If
        End If
        If builder IsNot Nothing Then
          builder.Add(newStatement)
        End If
      Next
      If builder Is Nothing Then
        Return node
      End If
      Return New BoundBlockStatement(builder.MoveToImmutable)
    End Function

    'Protected Overridable Function RewriteNopStatement(node As BoundNopStatement) As BoundStatement
    '  Return node
    'End Function

    Protected Overridable Function RewriteVariableDeclaration(node As BoundVariableDeclaration) As BoundStatement
      Dim initializer = RewriteExpression(node.Initializer)
      If initializer Is node.Initializer Then
        Return node
      End If
      Return New BoundVariableDeclaration(node.Variable, initializer)
    End Function

    Protected Overridable Function RewriteIfStatement(node As BoundIfStatement) As BoundStatement
      Dim condition = RewriteExpression(node.Condition)
      Dim thenStatements = RewriteStatements(node.IfStatements)
      Dim elseStatements = RewriteStatements(node.ElseStatements)
      If condition Is node.Condition AndAlso
         thenStatements.Equals(node.IfStatements) AndAlso
         elseStatements.Equals(node.ElseStatements) Then
        Return node
      End If
      Return New BoundIfStatement(condition, thenStatements, Nothing, elseStatements)
    End Function

    Protected Overridable Function RewriteWhileStatement(node As BoundWhileStatement) As BoundStatement
      Dim condition = RewriteExpression(node.Condition)
      Dim body = RewriteStatement(node.Body)
      If condition Is node.Condition AndAlso body Is node.Body Then
        Return node
      End If
      Return New BoundWhileStatement(condition, body) ', node.BreakLabel, node.ContinueLabel)
    End Function

    'Protected Overridable Function RewriteDoWhileStatement(node As BoundDoWhileStatement) As BoundStatement
    '  Dim body = RewriteStatement(node.Body)
    '  Dim condition = RewriteExpression(node.Condition)
    '  If body Is node.Body AndAlso condition Is node.Condition Then
    '    Return node
    '  End If
    '  Return New BoundDoWhileStatement(body, condition, node.BreakLabel, node.ContinueLabel)
    'End Function

    Protected Overridable Function RewriteForStatement(node As BoundForStatement) As BoundStatement
      Dim lowerBound = RewriteExpression(node.LowerBound)
      Dim upperBound = RewriteExpression(node.UpperBound)
      Dim stepper = RewriteExpression(node.Stepper)
      Dim body = RewriteStatement(node.Body)
      If lowerBound Is node.LowerBound AndAlso
         upperBound Is node.UpperBound AndAlso
         stepper Is node.Stepper AndAlso
         body Is node.Body Then
        Return node
      End If
      Return New BoundForStatement(node.Variable, lowerBound, upperBound, stepper, body) ', node.BreakLabel, node.ContinueLabel)
    End Function

    Protected Overridable Function RewriteLabeltatement(node As BoundLabelStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteGotoStatement(node As BoundGotoStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteConditionalGotoStatement(node As BoundConditionalGotoStatement) As BoundStatement
      Dim condition = RewriteExpression(node.Condition)
      If condition Is node.Condition Then
        Return node
      End If
      Return New BoundConditionalGotoStatement(node.Label, condition, node.JumpIfTrue)
    End Function

    'Protected Overridable Function RewriteReturnStatement(node As BoundReturnStatement) As BoundStatement
    '  Dim expression = If(node.Expression Is Nothing, Nothing, RewriteExpression(node.Expression))
    '  If expression Is node.Expression Then
    '    Return node
    '  End If
    '  Return New BoundReturnStatement(expression)
    'End Function

    Protected Overridable Function RewriteExpressionStatement(node As BoundExpressionStatement) As BoundStatement
      Dim expression = RewriteExpression(node.Expression)
      If expression Is node.Expression Then
        Return node
      Else
        Return New BoundExpressionStatement(expression)
      End If
    End Function

    Private Shared Function RewriteErrorExpression(node As BoundErrorExpression) As BoundExpression
      Return node
    End Function

    Protected Overridable Function RewriteLiteralExpression(node As BoundLiteralExpression) As BoundExpression
      Return node
    End Function

    Protected Overridable Function RewriteVariableExpression(node As BoundVariableExpression) As BoundExpression
      Return node
    End Function

    Protected Overridable Function RewriteAssignmentExpression(node As BoundAssignmentExpression) As BoundExpression
      Dim expression = RewriteExpression(node.Expression)
      If expression Is node.Expression Then
        Return node
      Else
        Return New BoundAssignmentExpression(node.Variable, expression)
      End If
    End Function

    Protected Overridable Function RewriteUnaryExpression(node As BoundUnaryExpression) As BoundExpression
      Dim operand = RewriteExpression(node.Operand)
      If operand Is node.Operand Then
        Return node
      Else
        Return New BoundUnaryExpression(node.Op, operand)
      End If
    End Function

    Protected Overridable Function RewriteBinaryExpression(node As BoundBinaryExpression) As BoundExpression
      Dim left = RewriteExpression(node.Left)
      Dim right = RewriteExpression(node.Right)
      If left Is node.Left AndAlso right Is node.Right Then
        Return node
      Else
        Return New BoundBinaryExpression(left, node.Op, right)
      End If
    End Function

    'Protected Overridable Function RewriteCallExpression(node As BoundCallExpression) As BoundExpression
    '  Dim builder As ImmutableArray(Of BoundExpression).Builder = Nothing
    '  For i = 0 To node.Arguments.Length - 1
    '    Dim oldArgument = node.Arguments(i)
    '    Dim newArgument = RewriteExpression(oldArgument)
    '    If newArgument IsNot oldArgument Then
    '      If builder Is Nothing Then
    '        builder = ImmutableArray.CreateBuilder(Of BoundExpression)(node.Arguments.Length)
    '        For j = 0 To i - 1
    '          builder.Add(node.Arguments(j))
    '        Next
    '      End If
    '    End If
    '    If builder IsNot Nothing Then
    '      builder.Add(newArgument)
    '    End If
    '  Next
    '  If builder Is Nothing Then
    '    Return node
    '  End If
    '  Return New BoundCallExpression(node.Function, builder.MoveToImmutable)
    'End Function

    'Protected Overridable Function RewriteConversionExpression(node As BoundConversionExpression) As BoundExpression
    '  Dim expression = RewriteExpression(node.Expression)
    '  If expression Is node.Expression Then
    '    Return node
    '  Else
    '    Return New BoundConversionExpression(node.Type, expression)
    '  End If
    'End Function

  End Class

End Namespace
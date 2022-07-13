Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Binding

  Friend MustInherit Class BoundTreeRewriter

    Public Overridable Function RewriteStatement(node As BoundStatement) As BoundStatement
      Select Case node.Kind
        Case BoundNodeKind.BlockStatement : Return RewriteBlockStatement(DirectCast(node, BoundBlockStatement))
        Case BoundNodeKind.ChDirStatement : Return RewriteChDirStatement(DirectCast(node, BoundChDirStatement))
        Case BoundNodeKind.ClearStatement : Return RewriteClearStatement(DirectCast(node, BoundClearStatement))
        Case BoundNodeKind.ClsStatement : Return RewriteClsStatement(DirectCast(node, BoundClsStatement))
        Case BoundNodeKind.ConditionalGotoStatement : Return RewriteConditionalGotoStatement(DirectCast(node, BoundConditionalGotoStatement))
        Case BoundNodeKind.DoUntilStatement : Return RewriteDoUntilStatement(DirectCast(node, BoundDoUntilStatement))
        Case BoundNodeKind.DoWhileStatement : Return RewriteDoWhileStatement(DirectCast(node, BoundDoWhileStatement))
        Case BoundNodeKind.EndStatement : Return RewriteEndStatement(DirectCast(node, BoundEndStatement))
        Case BoundNodeKind.ExpressionStatement : Return RewriteExpressionStatement(DirectCast(node, BoundExpressionStatement))
        Case BoundNodeKind.ForStatement : Return RewriteForStatement(DirectCast(node, BoundForStatement))
        Case BoundNodeKind.GosubStatement : Return RewriteGosubStatement(DirectCast(node, BoundGosubStatement))
        Case BoundNodeKind.GotoStatement : Return RewriteGotoStatement(DirectCast(node, BoundGotoStatement))
        Case BoundNodeKind.IfStatement : Return RewriteIfStatement(DirectCast(node, BoundIfStatement))
        Case BoundNodeKind.InputStatement : Return RewriteInputStatement(DirectCast(node, BoundInputStatement))
        Case BoundNodeKind.KillStatement : Return RewriteKillStatement(DirectCast(node, BoundKillStatement))
        Case BoundNodeKind.LabelStatement : Return RewriteLabelStatement(DirectCast(node, BoundLabelStatement))
        Case BoundNodeKind.LetStatement : Return RewriteLetStatement(DirectCast(node, BoundLetStatement))
        Case BoundNodeKind.MidStatement : Return RewriteMidStatement(DirectCast(node, BoundMidStatement))
        Case BoundNodeKind.MkDirStatement : Return RewriteMkDirStatement(DirectCast(node, BoundMkDirStatement))
        Case BoundNodeKind.NameStatement : Return RewriteNameStatement(DirectCast(node, BoundNameStatement))
        Case BoundNodeKind.NopStatement : Return RewriteNopStatement(DirectCast(node, BoundNopStatement))
        Case BoundNodeKind.OptionStatement : Return RewriteOptionStatement(DirectCast(node, BoundOptionStatement))
        Case BoundNodeKind.PrintStatement : Return RewritePrintStatement(DirectCast(node, BoundPrintStatement))
        Case BoundNodeKind.RemStatement : Return RewriteRemStatement(DirectCast(node, BoundRemStatement))
        Case BoundNodeKind.RmDirStatement : Return RewriteRmDirStatement(DirectCast(node, BoundRmDirStatement))
        Case BoundNodeKind.ReturnGosubStatement : Return RewriteReturnGosubStatement(DirectCast(node, BoundReturnGosubStatement))
        Case BoundNodeKind.ReturnStatement : Return RewriteReturnStatement(DirectCast(node, BoundReturnStatement))
        Case BoundNodeKind.StopStatement : Return RewriteStopStatement(DirectCast(node, BoundStopStatement))
        Case BoundNodeKind.SystemStatement : Return RewriteSystemStatement(DirectCast(node, BoundSystemStatement))
        Case BoundNodeKind.VariableDeclaration : Return RewriteVariableDeclaration(DirectCast(node, BoundVariableDeclaration))
        Case BoundNodeKind.WhileStatement : Return RewriteWhileStatement(DirectCast(node, BoundWhileStatement))
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
        Case BoundNodeKind.CallExpression : Return RewriteCallExpression(DirectCast(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : Return RewriteConversionExpression(DirectCast(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node: {node.Kind}")
      End Select
    End Function

    'Protected Overridable Function RewriteStatements(statements As ImmutableArray(Of BoundStatement)) As ImmutableArray(Of BoundStatement)
    '  Dim builder As ImmutableArray(Of BoundStatement).Builder = Nothing
    '  For i = 0 To statements.Length - 1
    '    Dim oldStatement = statements(i)
    '    Dim newStatement = RewriteStatement(oldStatement)
    '    If newStatement IsNot oldStatement Then
    '      If builder Is Nothing Then
    '        builder = ImmutableArray.CreateBuilder(Of BoundStatement)(statements.Length)
    '        For j = 0 To i - 1
    '          builder.Add(statements(j))
    '        Next
    '      End If
    '    End If
    '    If builder IsNot Nothing Then
    '      builder.Add(newStatement)
    '    End If
    '  Next
    '  If builder Is Nothing Then
    '    Return statements
    '  End If
    '  Return builder.MoveToImmutable
    'End Function
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

    Protected Overridable Function RewriteChDirStatement(node As BoundChDirStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteClearStatement(node As BoundClearStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteClsStatement(node As BoundClsStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteStopStatement(node As BoundStopStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteSystemStatement(node As BoundSystemStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteMkDirStatement(node As BoundMkDirStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteInputStatement(node As BoundInputStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteKillStatement(node As BoundKillStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteNameStatement(node As BoundNameStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteRmDirStatement(node As BoundRmDirStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteOptionStatement(node As BoundOptionStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewritePrintStatement(node As BoundPrintStatement) As BoundStatement

      'TODO: need to lower the parameter(s) of
      '      the print statement into individual, 
      '      simplistic `print [expression][;]`
      '      statements.

      ' PRINT "1+1 ="; Spc(1); 1+1; Tab(20); " <--- calculator", "X"
      ' 
      ' becomes
      '
      ' HandlePrint "1+1="
      ' HandleSpc 1
      ' HandlePrint 1+1
      ' HandleTab 20
      ' HandlePrint " <--- calculator"
      ' HandleComma
      ' HandlePrint "X"
      ' HandlePrintLine

      ' This will allow for a simpler conversion (compile)
      ' when it comes time to write IL as it allows for..

      ' System.Console.Write("1+1=")
      ' BSharp.Runtime.HandleSpc(1)
      ' System.Console.Write(1+1)
      ' BSharp.Runtime.HandleTab(20)
      ' System.Console.Write(" <--- calculator")
      ' BSharp.Runtime.HandleComma()
      ' System.Console.Write("X")
      ' System.Console.WriteLine()

      ' Not the most efficient, but it should work and, in the end,
      ' we are talking about a PRINT statement afterall.

      'Return node

      'Dim builder As ImmutableArray(Of BoundStatement).Builder = Nothing
      Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)()
      Dim cr = False
      For Each entry In node.Nodes
        If TypeOf entry Is BoundSymbol Then
          Select Case CType(entry, BoundSymbol).Value
            Case ";"c
              cr = False
            Case ","
              ' Convert to a HandleComma statement.
              builder.Add(New BoundHandleCommaStatement())
              cr = False
            Case Else
              cr = True
          End Select
        ElseIf TypeOf entry Is BoundSpcFunction Then
          Dim expression = RewriteExpression(CType(entry, BoundSpcFunction).Expression)
          builder.Add(New BoundHandleSpcStatement(expression))
          cr = False
        ElseIf TypeOf entry Is BoundTabFunction Then
          Dim expression = RewriteExpression(CType(entry, BoundTabFunction).Expression)
          builder.Add(New BoundHandleTabStatement(expression))
          cr = False
        Else
          Dim expression = RewriteExpression(CType(entry, BoundExpression))
          builder.Add(New BoundHandlePrintStatement(expression))
          cr = True
        End If
      Next
      If cr Then
        builder.Add(New BoundHandlePrintLineStatement())
      End If
      If builder Is Nothing Then
        Return node
      End If
      Return New BoundBlockStatement(builder.ToImmutable)

      'Dim screenWidth = 80
      'Dim zoneWidth = 14

      'Dim cr = True
      'For Each entry In node.Nodes
      '  If TypeOf entry Is BoundSymbol Then
      '    Select Case CType(entry, BoundSymbol).Value
      '      Case ";"c
      '        cr = False
      '      Case ","
      '        ' Convert to a HandleComma statement.
      '        Dim pos = Console.CursorLeft + 1
      '        Dim cur = pos Mod zoneWidth
      '        Console.Write(Microsoft.VisualBasic.Strings.Space(cur))
      '        cr = False
      '      Case Else
      '        Throw New NotImplementedException
      '        cr = True
      '    End Select
      '  ElseIf TypeOf entry Is BoundSpcFunction Then
      '    ' Convert to a HandleSpc(*number*) statement.
      '    Dim result = EvaluateExpression(CType(entry, BoundSpcFunction).Expression)
      '    Dim value = CInt(result)
      '    If value < 0 OrElse value > 255 Then
      '      'error
      '    ElseIf value > screenWidth Then
      '      value = value Mod screenWidth
      '    End If
      '    Dim str = Microsoft.VisualBasic.Strings.Space(value)
      '    Console.Write(str)
      '    cr = False
      '  ElseIf TypeOf entry Is BoundTabFunction Then
      '    ' Convert to a HandleTab(*number*) statement.
      '    Dim result = EvaluateExpression(CType(entry, BoundTabFunction).Expression)
      '    Dim value = CInt(result)
      '    If value < 0 OrElse value > 255 Then
      '      ' error
      '    End If
      '    Dim pos = Console.CursorLeft + 1
      '    Dim diff = 0
      '    If pos < value Then
      '      diff = value - pos
      '    ElseIf pos > value Then
      '      diff = screenWidth - pos
      '      Console.WriteLine(Microsoft.VisualBasic.Strings.Space(diff))
      '      diff = value
      '    End If
      '    Dim str = Microsoft.VisualBasic.Strings.Space(diff)
      '    Console.Write(str)
      '  Else
      '    Dim value = EvaluateExpression(CType(entry, BoundExpression))
      '    Dim str = ""
      '    If TypeOf value Is BoundConstant Then
      '      str = CStr(CType(value, BoundConstant).Value)
      '    Else
      '      str = CStr(value)
      '    End If
      '    Console.Write(str)
      '    cr = True
      '  End If
      'Next
      'If cr Then
      '  ' PRINT ' To handle CR
      '  Console.WriteLine()
      'End If

    End Function

    Protected Overridable Function RewriteLetStatement(node As BoundLetStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteMidStatement(node As BoundMidStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteNopStatement(node As BoundNopStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteRemStatement(node As BoundRemStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteVariableDeclaration(node As BoundVariableDeclaration) As BoundStatement
      Dim initializer = RewriteExpression(node.Initializer)
      If initializer Is node.Initializer Then
        Return node
      End If
      Return New BoundVariableDeclaration(node.Variable, initializer)
    End Function

    Protected Overridable Function RewriteIfStatement(node As BoundIfStatement) As BoundStatement
      Dim condition = RewriteExpression(node.Expression)
      Dim thenStatement = RewriteStatement(node.Statements)
      Dim elseStatement = RewriteStatement(node.ElseStatement)
      If condition Is node.Expression AndAlso
         thenStatement.Equals(node.Statements) AndAlso
         elseStatement.Equals(node.ElseStatement) Then
        Return node
      End If
      Return New BoundIfStatement(condition, thenStatement, elseStatement)
    End Function

    Protected Overridable Function RewriteWhileStatement(node As BoundWhileStatement) As BoundStatement
      Dim condition = RewriteExpression(node.Expression)
      Dim body = RewriteStatement(node.Statements)
      If condition Is node.Expression AndAlso body Is node.Statements Then
        Return node
      End If
      Return New BoundWhileStatement(condition, body, node.ExitLabel, node.ContinueLabel)
    End Function

    Protected Overridable Function RewriteDoWhileStatement(node As BoundDoWhileStatement) As BoundStatement
      Dim body = RewriteStatement(node.Statements)
      Dim condition = RewriteExpression(node.Expression)
      Dim atBeginning = node.AtBeginning
      If body Is node.Statements AndAlso condition Is node.Expression Then
        Return node
      End If
      Return New BoundDoWhileStatement(body, condition, atBeginning, node.ExitLabel, node.ContinueLabel)
    End Function

    Protected Overridable Function RewriteDoUntilStatement(node As BoundDoUntilStatement) As BoundStatement
      Dim body = RewriteStatement(node.Statements)
      Dim condition = RewriteExpression(node.Expression)
      Dim atBeginning = node.AtBeginning
      If body Is node.Statements AndAlso condition Is node.Expression Then
        Return node
      End If
      Return New BoundDoUntilStatement(body, condition, atBeginning, node.ExitLabel, node.ContinueLabel)
    End Function

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
      Return New BoundForStatement(node.Variable, lowerBound, upperBound, stepper, body, node.ExitLabel, node.ContinueLabel)
    End Function

    Protected Overridable Function RewriteLabelStatement(node As BoundLabelStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteGosubStatement(node As BoundGosubStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteGotoStatement(node As BoundGotoStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteEndStatement(node As BoundEndStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteConditionalGotoStatement(node As BoundConditionalGotoStatement) As BoundStatement
      Dim condition = RewriteExpression(node.Condition)
      If condition Is node.Condition Then
        Return node
      End If
      Return New BoundConditionalGotoStatement(node.Label, condition, node.JumpIfTrue)
    End Function

    Protected Overridable Function RewriteReturnGosubStatement(node As BoundReturnGosubStatement) As BoundStatement
      Return node
    End Function

    Protected Overridable Function RewriteReturnStatement(node As BoundReturnStatement) As BoundStatement
      Dim expression = If(node.Expression Is Nothing, Nothing, RewriteExpression(node.Expression))
      If expression Is node.Expression Then
        Return node
      End If
      Return New BoundReturnStatement(expression)
    End Function

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

    Protected Overridable Function RewriteCallExpression(node As BoundCallExpression) As BoundExpression
      Dim builder As ImmutableArray(Of BoundExpression).Builder = Nothing
      For i = 0 To node.Arguments.Length - 1
        Dim oldArgument = node.Arguments(i)
        Dim newArgument = RewriteExpression(oldArgument)
        If newArgument IsNot oldArgument Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of BoundExpression)(node.Arguments.Length)
            For j = 0 To i - 1
              builder.Add(node.Arguments(j))
            Next
          End If
        End If
        If builder IsNot Nothing Then
          builder.Add(newArgument)
        End If
      Next
      If builder Is Nothing Then
        Return node
      End If
      Return New BoundCallExpression(node.Function, builder.MoveToImmutable)
    End Function

    Protected Overridable Function RewriteConversionExpression(node As BoundConversionExpression) As BoundExpression
      Dim expression = RewriteExpression(node.Expression)
      If expression Is node.Expression Then
        Return node
      Else
        Return New BoundConversionExpression(node.Type, expression)
      End If
    End Function

  End Class

End Namespace
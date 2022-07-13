Imports Bsharp.CodeAnalysis.Binding
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis

  Friend NotInheritable Class Evaluator

    Private ReadOnly m_program As BoundProgram
    Private ReadOnly m_globals As Dictionary(Of VariableSymbol, Object)
    Private ReadOnly m_functions As New Dictionary(Of FunctionSymbol, BoundBlockStatement)
    Private ReadOnly m_locals As New Stack(Of Dictionary(Of VariableSymbol, Object))
    Private m_random As Random

    'TODO: Need to make this scoped.
    Private ReadOnly m_gosubStack As New Stack(Of Integer)

    Private m_lastValue As Object

    Sub New(program As BoundProgram, variables As Dictionary(Of VariableSymbol, Object))

      m_program = program
      m_globals = variables
      m_locals.Push(New Dictionary(Of VariableSymbol, Object))

      Dim currrent = program
      While currrent IsNot Nothing
        For Each kv In currrent.Functions
          Dim func = kv.Key
          Dim body = kv.Value
          m_functions.Add(func, body)
        Next
        currrent = currrent.Previous
      End While

    End Sub

    Public Function Evaluate() As Object
      Dim func = If(m_program.MainFunction, m_program.ScriptFunction)
      If func Is Nothing Then
        Return Nothing
      Else
        Dim body = m_functions(func)
        Return EvaluateStatement(body)
      End If
    End Function

    Private Function EvaluateStatement(body As BoundBlockStatement) As Object

      Dim labelToIndex = New Dictionary(Of BoundLabel, Integer)

      For i = 0 To body.Statements.Length - 1
        If TypeOf body.Statements(i) Is BoundLabelStatement Then
          labelToIndex.Add(CType(body.Statements(i), BoundLabelStatement).Label, i + 1)
        End If
      Next

      Dim index = 0
      While index < body.Statements.Length
        Dim s = body.Statements(index)
        Select Case s.Kind
          Case BoundNodeKind.ChDirStatement
            Dim chdir = CType(s, BoundChDirStatement)
            Dim value = CStr(EvaluateExpression(chdir.Expression))
            System.IO.Directory.SetCurrentDirectory(value)
            index += 1
          Case BoundNodeKind.ClearStatement
            index += 1
          Case BoundNodeKind.ClsStatement
            Dim cs = CType(s, BoundClsStatement)
            Dim value = If(cs.Expression Is Nothing, 0, CInt(EvaluateExpression(cs.Expression)))
            If value < 0 OrElse value > 2 Then
              ' error condition
              Console.WriteLine("CLS parameter out-of-range.")
            Else
              Select Case value
                Case 0 ' Clears the screen of all text and graphics
                  Console.Clear()
                Case 1 ' Clears only the graphics viewport
                  'TODO: Revisit after SCREEN, WIDTH, VIEW.
                  Console.Clear()
                Case 2 ' Clears only the text window
                  'TODO: Revisit after SCREEN, WIDTH, VIEW.
                  Console.Clear()
              End Select
            End If
            index += 1
          Case BoundNodeKind.ConditionalGotoStatement
            Dim cgs = CType(s, BoundConditionalGotoStatement)
            Dim condition = CBool(EvaluateExpression(cgs.Condition))
            If condition = cgs.JumpIfTrue Then
              index = labelToIndex(cgs.Label)
            Else
              index += 1
            End If
          Case BoundNodeKind.EndStatement
            index = body.Statements.Length
          Case BoundNodeKind.ExpressionStatement : EvaluateExpressionStatement(CType(s, BoundExpressionStatement)) : index += 1

          Case BoundNodeKind.GosubStatement
            Dim gs = CType(s, BoundGosubStatement)
            If labelToIndex.ContainsKey(gs.Label) Then
              m_gosubStack.Push(index + 1)
              index = labelToIndex(gs.Label)
            Else
              For Each entry In labelToIndex.Keys
                If entry.Name = gs.Label.Name Then
                  m_gosubStack.Push(index + 1)
                  index = labelToIndex(entry)
                  Exit For
                End If
              Next
            End If

          Case BoundNodeKind.GotoStatement
            Dim gs = CType(s, BoundGotoStatement)

            If labelToIndex.ContainsKey(gs.Label) Then
              index = labelToIndex(gs.Label)
            Else
              For Each entry In labelToIndex.Keys
                If entry.Name = gs.Label.Name Then
                  index = labelToIndex(entry)
                  Exit For
                End If
              Next
            End If
            'index = labelToIndex(gs.Label)

          Case BoundNodeKind.HandleCommaStatement : EvaluateHandleCommaStatement(CType(s, BoundHandleCommaStatement)) : index += 1
          Case BoundNodeKind.HandlePrintLineStatement : EvaluateHandlePrintLineStatement(CType(s, BoundHandlePrintLineStatement)) : index += 1
          Case BoundNodeKind.HandlePrintStatement : EvaluateHandlePrintStatement(CType(s, BoundHandlePrintStatement)) : index += 1
          Case BoundNodeKind.HandleSpcStatement : EvaluateHandleSpcStatement(CType(s, BoundHandleSpcStatement)) : index += 1
          Case BoundNodeKind.HandleTabStatement : EvaluateHandleTabStatement(CType(s, BoundHandleTabStatement)) : index += 1

          Case BoundNodeKind.InputStatement

            Dim input = CType(s, BoundInputStatement)
            Dim suppressCr = input.SuppressCr
            Dim suppressQuestionMark = input.SuppressQuestionMark
            Dim prompt As String = Nothing
            If input.PromptExpression IsNot Nothing Then
              Dim value = CStr(EvaluateExpression(input.PromptExpression))
              prompt = value
            End If
            Do
              If Not String.IsNullOrEmpty(prompt) Then
                Console.Write(prompt)
              End If
              If Not suppressQuestionMark Then
                Console.Write("? ")
              End If
              Dim potential = Console.ReadLine()
              Dim potentials = Split(potential, ",")
              If potentials.Length = input.Variables.Length Then
                For i = 0 To input.Variables.Length - 1
                  Dim value = potentials(i)
                  If input.Variables(i).Type Is TypeSymbol.Double OrElse
                     input.Variables(i).Type Is TypeSymbol.Single OrElse
                     input.Variables(i).Type Is TypeSymbol.ULong64 OrElse
                     input.Variables(i).Type Is TypeSymbol.Long64 OrElse
                     input.Variables(i).Type Is TypeSymbol.ULong OrElse
                     input.Variables(i).Type Is TypeSymbol.Long OrElse
                     input.Variables(i).Type Is TypeSymbol.UInteger OrElse
                     input.Variables(i).Type Is TypeSymbol.Integer OrElse
                     input.Variables(i).Type Is TypeSymbol.SByte OrElse
                     input.Variables(i).Type Is TypeSymbol.Byte Then
                    If IsNumeric(value) Then
                      If value.Contains("."c) Then
                        If input.Variables(i).Type Is TypeSymbol.Single OrElse
                           input.Variables(i).Type Is TypeSymbol.Double Then
                          Assign(input.Variables(i), value)
                        Else
                          Console.WriteLine() : Continue Do
                        End If
                      Else
                        'TODO: Check in-range for values/types.
                        Assign(input.Variables(i), value)
                      End If
                    Else
                      Console.WriteLine() : Continue Do
                    End If
                  Else
                    Assign(input.Variables(i), value)
                  End If
                Next
                'If Not suppressCr Then Console.WriteLine()
                'TODO: If suppressCr is True, need to move the cursor back????
                Exit Do
              Else
                Console.WriteLine()
              End If
            Loop

            index += 1

          Case BoundNodeKind.KillStatement
            Dim kill = CType(s, BoundKillStatement)
            Dim value = CStr(EvaluateExpression(kill.Expression))
            If System.IO.Directory.Exists(value) Then
              System.IO.Directory.Delete(value)
            Else
              System.IO.File.Delete(value)
            End If
            index += 1
          Case BoundNodeKind.LabelStatement : index += 1
          Case BoundNodeKind.MidStatement : EvaluateMidStatement(CType(s, BoundMidStatement)) : index += 1
          Case BoundNodeKind.MkDirStatement
            Dim mkdir = CType(s, BoundMkDirStatement)
            Dim value = CStr(EvaluateExpression(mkdir.Expression))
            System.IO.Directory.CreateDirectory(value)
            index += 1
          Case BoundNodeKind.NameStatement
            Dim name = CType(s, BoundNameStatement)
            Dim originalPath = CStr(EvaluateExpression(name.OriginalPath))
            Dim destinationPath = CStr(EvaluateExpression(name.DestinationPath))
            If System.IO.Directory.Exists(originalPath) Then
              System.IO.Directory.Move(originalPath, destinationPath)
            Else
              System.IO.File.Move(originalPath, destinationPath)
            End If
            index += 1
          Case BoundNodeKind.NopStatement : index += 1
          Case BoundNodeKind.LetStatement : EvaluateLetStatement(CType(s, BoundLetStatement)) : index += 1
          Case BoundNodeKind.OptionStatement
            'TODO: Need to handle with Arrays.
            'TODO: Also need to track that no other invalid
            '      statements have executed (pretty much all
            '      other statements are *invalid* in this
            '      context.)
            index += 1

          Case BoundNodeKind.RemStatement : index += 1

          Case BoundNodeKind.ReturnGosubStatement
            Dim rg = CType(s, BoundReturnGosubStatement)

            If rg.Label Is Nothing Then
              If m_gosubStack.Count > 0 Then
                index = m_gosubStack.Pop
              End If
            ElseIf labelToIndex.ContainsKey(rg.Label) Then
              index = labelToIndex(rg.Label)
            Else
              For Each entry In labelToIndex.Keys
                If entry.Name = rg.Label.Name Then
                  index = labelToIndex(entry)
                  Exit For
                End If
              Next
            End If

          Case BoundNodeKind.ReturnStatement
            'TODO: Need to determine if this is a 
            '      value Return or a Return related
            '      to a Gosub.
            Dim rs = CType(s, BoundReturnStatement)
            m_lastValue = If(rs.Expression Is Nothing, Nothing, EvaluateExpression(rs.Expression))
            Return m_lastValue
          Case BoundNodeKind.RmDirStatement
            Dim rmdir = CType(s, BoundRmDirStatement)
            Dim value = CStr(EvaluateExpression(rmdir.Expression))
            System.IO.Directory.Delete(value)
            index += 1
          Case BoundNodeKind.StopStatement
            index = body.Statements.Length
          Case BoundNodeKind.SystemStatement
            index = body.Statements.Length
            m_lastValue = UInt64.MaxValue
          Case BoundNodeKind.VariableDeclaration : EvaluateVariableDeclaration(CType(s, BoundVariableDeclaration)) : index += 1
          Case Else
            Throw New Exception($"Unexpected kind {s.Kind}")
        End Select
      End While

      Return m_lastValue

    End Function

    Private Sub EvaluateMidStatement(node As BoundMidStatement)
      Dim positionValue = CInt(EvaluateExpression(node.PositionExpression))
      Dim lengthValue = CInt(If(node.LengthExpression Is Nothing, Integer.MaxValue, EvaluateExpression(node.LengthExpression)))
      Dim value = CStr(EvaluateExpression(node.Expression))
      'Assign(node.Variable, value)
      If node.Variable.Kind = SymbolKind.GlobalVariable Then
        Dim temp = CStr(m_globals(node.Variable))
        Mid(temp, positionValue, lengthValue) = value
        m_globals(node.Variable) = temp
      Else
        Dim locals = m_locals.Peek
        locals(node.Variable) = value
        Dim temp = CStr(locals(node.Variable))
        Mid(temp, positionValue, lengthValue) = value
        locals(node.Variable) = temp
      End If
    End Sub

    Private Sub EvaluateLetStatement(node As BoundLetStatement)
      Dim value = EvaluateExpression(node.Expression)
      Debug.Assert(value IsNot Nothing)
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    Private Shared Sub EvaluateHandleCommaStatement(node As BoundHandleCommaStatement)
      If node IsNot Nothing Then
      End If
      'Dim screenWidth = 80
      Dim zoneWidth = 14
      Dim pos = Console.CursorLeft + 1
      Dim cur = pos Mod zoneWidth
      Console.Write(Microsoft.VisualBasic.Strings.Space(cur))
    End Sub

    Private Shared Sub EvaluateHandlePrintLineStatement(node As BoundHandlePrintLineStatement)
      If node IsNot Nothing Then
      End If
      Console.WriteLine()
    End Sub

    Private Sub EvaluateHandlePrintStatement(node As BoundHandlePrintStatement)
      Dim value = EvaluateExpression(node.Expression)
      Dim str As String '= ""
      If TypeOf value Is BoundConstant Then
        str = CStr(CType(value, BoundConstant).Value)
      Else
        str = CStr(value)
      End If
      Console.Write(str) : Console.Write(" "c)
    End Sub

    Private Sub EvaluateHandleSpcStatement(node As BoundHandleSpcStatement)
      Dim screenWidth = 80
      'Dim zoneWidth = 14
      Dim result = EvaluateExpression(node.Expression)
      Dim value = CInt(result)
      If value < 0 OrElse value > 255 Then
        'error
      ElseIf value > screenWidth Then
        value = value Mod screenWidth
      End If
      Dim str = Microsoft.VisualBasic.Strings.Space(value)
      Console.Write(str)
    End Sub

    Private Sub EvaluateHandleTabStatement(node As BoundHandleTabStatement)
      Dim screenWidth = 80
      'Dim zoneWidth = 14
      Dim result = EvaluateExpression(node.Expression)
      Dim value = CInt(result)
      If value < 0 OrElse value > 255 Then
        ' error
      End If
      Dim pos = Console.CursorLeft + 1
      Dim diff = 0
      If pos < value Then
        diff = value - pos
      ElseIf pos > value Then
        diff = screenWidth - pos
        Console.WriteLine(Microsoft.VisualBasic.Strings.Space(diff))
        diff = value
      End If
      Dim str = Microsoft.VisualBasic.Strings.Space(diff)
      Console.Write(str)
    End Sub

    Private Sub EvaluateVariableDeclaration(node As BoundVariableDeclaration)
      Dim value = EvaluateExpression(node.Initializer)
      Debug.Assert(value IsNot Nothing)
      m_lastValue = value
      Assign(node.Variable, value)
    End Sub

    Private Sub EvaluateExpressionStatement(node As BoundExpressionStatement)
      m_lastValue = EvaluateExpression(node.Expression)
    End Sub

    Private Function EvaluateExpression(node As BoundExpression) As Object

      If node.ConstantValue IsNot Nothing Then
        Return EvaluateConstantExpression(node)
      End If

      Select Case node.Kind
        'Case BoundNodeKind.LiteralExpression : Return EvaluateLiteralExpression(CType(node, BoundLiteralExpression))
        Case BoundNodeKind.VariableExpression : Return EvaluateVariableExpression(CType(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : Return EvaluateAssignmentExpression(CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : Return EvaluateUnaryExpression(CType(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : Return EvaluateBinaryExpression(CType(node, BoundBinaryExpression))
        Case BoundNodeKind.CallExpression : Return EvaluateCallExpression(CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : Return EvaluateConversionExpression(CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node {node.Kind}")
      End Select

    End Function

    Private Shared Function EvaluateConstantExpression(node As BoundExpression) As Object
      Debug.Assert(node.ConstantValue IsNot Nothing)
      Return node.ConstantValue.Value
    End Function

    Private Function EvaluateVariableExpression(node As BoundVariableExpression) As Object
      If node.Variable.Kind = SymbolKind.GlobalVariable Then
        Return m_globals(node.Variable)
      Else
        Dim locals = m_locals.Peek
        Return locals(node.Variable)
      End If
    End Function

    Private Function EvaluateAssignmentExpression(node As BoundAssignmentExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      Debug.Assert(value IsNot Nothing)
      Assign(node.Variable, value)
      Return value
    End Function

    Private Function EvaluateUnaryExpression(node As BoundUnaryExpression) As Object
      Dim operand = EvaluateExpression(node.Operand)
      Select Case node.Op.Kind
        Case BoundUnaryOperatorKind.Identity : Return CInt(operand)
        Case BoundUnaryOperatorKind.Negation : Return -CInt(operand)
        Case BoundUnaryOperatorKind.LogicalNegation : Return Not CBool(operand)
        Case BoundUnaryOperatorKind.BitwiseComplement : Return Not CInt(operand)
        Case Else
          Throw New Exception($"Unexpected unary operator {node.Op}")
      End Select
    End Function

    Private Function EvaluateBinaryExpression(node As BoundBinaryExpression) As Object
      Dim left = EvaluateExpression(node.Left)
      Dim right = EvaluateExpression(node.Right)

      Debug.Assert(left IsNot Nothing AndAlso right IsNot Nothing)
      Select Case node.Op.Kind

' 14 ()
' 13 ^
' 12 - (negation "unary")
' 11 */
' 10 \
' 09 MOD
' 08 +-
' 07 = > >= < <= <>
' 06 NOT
' 05 AND, AndAlso
' 04 OR, OrElse
' 03 XOR
' 02 EQV
' 01 IMP

        Case BoundBinaryOperatorKind.Raise
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) ^ CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) ^ CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) ^ CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) ^ CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) ^ CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) ^ CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) ^ CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) ^ CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) ^ CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) ^ CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) ^ CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Multiplication
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) * CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) * CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) * CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) * CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) * CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) * CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) * CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) * CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) * CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) * CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) * CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Division
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(CDec(left) / CDec(right)))
            Case TypeSymbol.Type.Double : Return (CDbl(CDbl(left) / CDbl(right)))
            Case TypeSymbol.Type.Single : Return (CSng(CSng(left) / CSng(right)))
            Case TypeSymbol.Type.ULong64 : Return (CULng(CULng(left) / CULng(right)))
            Case TypeSymbol.Type.Long64 : Return (CLng(CLng(left) / CLng(right)))
            Case TypeSymbol.Type.ULong : Return (CUInt(CUInt(left) / CUInt(right)))
            Case TypeSymbol.Type.Long : Return (CInt(CInt(left) / CInt(right)))
            Case TypeSymbol.Type.UInteger : Return (CUShort(CUShort(left) / CUShort(right)))
            Case TypeSymbol.Type.Integer : Return (CShort(CShort(left) / CShort(right)))
            Case TypeSymbol.Type.SByte : Return (CSByte(CSByte(left) / CSByte(right)))
            Case TypeSymbol.Type.Byte : Return (CByte(CByte(left) / CByte(right)))
          End Select

        Case BoundBinaryOperatorKind.IntegerDivision
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) \ CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) \ CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) \ CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) \ CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) \ CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) \ CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) \ CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) \ CByte(right))
          End Select

        Case BoundBinaryOperatorKind.ModOperation
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) Mod CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) Mod CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) Mod CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) Mod CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) Mod CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) Mod CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) Mod CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) Mod CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) Mod CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) Mod CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) Mod CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Addition
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) + CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) + CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) + CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) + CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) + CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) + CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) + CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) + CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) + CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) + CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) + CByte(right))
            Case TypeSymbol.Type.String : Return (CStr(left) & CStr(right))
          End Select

        Case BoundBinaryOperatorKind.Subtraction
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) - CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) - CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) - CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) - CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) - CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) - CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) - CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) - CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) - CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) - CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) - CByte(right))
          End Select

        Case BoundBinaryOperatorKind.Equal
          Return Equals(left, right)
        Case BoundBinaryOperatorKind.NotEqual
          Return Not Equals(left, right)

        Case BoundBinaryOperatorKind.GreaterThan
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) > CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) > CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) > CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) > CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) > CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) > CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) > CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) > CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) > CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) > CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) > CByte(right))
            Case TypeSymbol.Type.String : Return (CStr(left) > CStr(right))
          End Select

        Case BoundBinaryOperatorKind.GreaterThanEqual
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) >= CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) >= CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) >= CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) >= CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) >= CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) >= CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) >= CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) >= CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) >= CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) >= CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) >= CByte(right))
            Case TypeSymbol.Type.String : Return (CStr(left) >= CStr(right))
          End Select

        Case BoundBinaryOperatorKind.LessThan
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) < CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) < CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) < CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) < CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) < CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) < CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) < CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) < CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) < CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) < CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) < CByte(right))
            Case TypeSymbol.Type.String : Return (CStr(left) < CStr(right))
          End Select

        Case BoundBinaryOperatorKind.LessThanEqual
          Select Case TypeSymbol.TypeSymbolToType(node.Left.Type)
            Case TypeSymbol.Type.Decimal : Return (CDec(left) <= CDec(right))
            Case TypeSymbol.Type.Double : Return (CDbl(left) <= CDbl(right))
            Case TypeSymbol.Type.Single : Return (CSng(left) <= CSng(right))
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) <= CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) <= CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) <= CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) <= CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) <= CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) <= CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) <= CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) <= CByte(right))
            Case TypeSymbol.Type.String : Return (CStr(left) <= CStr(right))
          End Select

        Case BoundBinaryOperatorKind.LogicalAnd, BoundBinaryOperatorKind.BitwiseAnd
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) And CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) And CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) And CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) And CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) And CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) And CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) And CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) And CByte(right))
            Case TypeSymbol.Type.Boolean : Return (CBool(left) And CBool(right))
          End Select

        Case BoundBinaryOperatorKind.LogicalAndAlso
          Return CBool(left) AndAlso CBool(right)

        Case BoundBinaryOperatorKind.LogicalOr, BoundBinaryOperatorKind.BitwiseOr
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) Or CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) Or CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) Or CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) Or CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) Or CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) Or CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) Or CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) Or CByte(right))
            Case TypeSymbol.Type.Boolean : Return (CBool(left) Or CBool(right))
          End Select

        Case BoundBinaryOperatorKind.LogicalOrElse
          Return CBool(left) OrElse CBool(right)

        Case BoundBinaryOperatorKind.LogicalXor, BoundBinaryOperatorKind.BitwiseXor
          Select Case TypeSymbol.TypeSymbolToType(node.Type)
            Case TypeSymbol.Type.ULong64 : Return (CULng(left) Xor CULng(right))
            Case TypeSymbol.Type.Long64 : Return (CLng(left) Xor CLng(right))
            Case TypeSymbol.Type.ULong : Return (CUInt(left) Xor CUInt(right))
            Case TypeSymbol.Type.Long : Return (CInt(left) Xor CInt(right))
            Case TypeSymbol.Type.UInteger : Return (CUShort(left) Xor CUShort(right))
            Case TypeSymbol.Type.Integer : Return (CShort(left) Xor CShort(right))
            Case TypeSymbol.Type.SByte : Return (CSByte(left) Xor CSByte(right))
            Case TypeSymbol.Type.Byte : Return (CByte(left) Xor CByte(right))
            Case TypeSymbol.Type.Boolean : Return (CBool(left) Xor CBool(right))
          End Select

        Case BoundBinaryOperatorKind.BitwiseEqv : Return CBool(left) = CBool(right)
        Case BoundBinaryOperatorKind.BitwiseImp : Return CBool(left) AndAlso Not CBool(right)

        Case Else
          Throw New Exception($"Unexpected binary operator {node.Op.Kind}")
      End Select

      Throw New Exception($"Unexpected binary operator {left} {node.Op.Kind} {right} type {node.Type}.")

    End Function

    Private Function EvaluateCallExpression(node As BoundCallExpression) As Object
      If node.Function Is BuiltinFunctions.Abs Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Abs(value)
      ElseIf node.Function Is BuiltinFunctions.Asc Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Strings.Asc(value)
      ElseIf node.Function Is BuiltinFunctions.Atn Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Atan(value)
      ElseIf node.Function Is BuiltinFunctions.Chr Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Strings.Chr(value)
      ElseIf node.Function Is BuiltinFunctions.Cos Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Cos(value)
      ElseIf node.Function Is BuiltinFunctions.Hex Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Hex(value)
        'ElseIf node.[Function] Is BuiltinFunctions.Input Then
        '  Return Console.ReadLine()
      ElseIf node.Function Is BuiltinFunctions.Instr1 Then
        Dim string1 = CStr(EvaluateExpression(node.Arguments(0)))
        Dim string2 = CStr(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.InStr(string1, string2)
      ElseIf node.Function Is BuiltinFunctions.Instr2 Then
        Dim position = CInt(EvaluateExpression(node.Arguments(0)))
        Dim string1 = CStr(EvaluateExpression(node.Arguments(1)))
        Dim string2 = CStr(EvaluateExpression(node.Arguments(2)))
        Return Microsoft.VisualBasic.InStr(position, string1, string2)
      ElseIf node.Function Is BuiltinFunctions.Int Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Int(value)
      ElseIf node.Function Is BuiltinFunctions.LBound Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.LCase Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return value?.ToLower
      ElseIf node.Function Is BuiltinFunctions.Left Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim position = CInt(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.Left(value, position)
      ElseIf node.Function Is BuiltinFunctions.Len Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Len(value)
      ElseIf node.Function Is BuiltinFunctions.Log Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Log(value)
      ElseIf node.Function Is BuiltinFunctions.Mid1 Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim start = CInt(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.Mid(value, start)
      ElseIf node.Function Is BuiltinFunctions.Mid2 Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim start = CInt(EvaluateExpression(node.Arguments(1)))
        Dim length = CInt(EvaluateExpression(node.Arguments(2)))
        Return Microsoft.VisualBasic.Mid(value, start, length)
      ElseIf node.Function Is BuiltinFunctions.Oct Then
        Dim value = CInt(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Oct(value)
      ElseIf node.Function Is BuiltinFunctions.Right Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Dim position = CInt(EvaluateExpression(node.Arguments(1)))
        Return Microsoft.VisualBasic.Right(value, position)
      ElseIf node.[Function] Is BuiltinFunctions.Rnd Then
        Dim max = CInt(EvaluateExpression(node.Arguments(0)))
        If m_random Is Nothing Then m_random = New Random
        Return m_random.[Next](max)
      ElseIf node.Function Is BuiltinFunctions.Sgn Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Sign(value)
      ElseIf node.Function Is BuiltinFunctions.Sin Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Sin(value)
      ElseIf node.Function Is BuiltinFunctions.Space Then
        Dim length = CInt(EvaluateExpression(node.Arguments(0)))
        Return New String(" "c, length)
      ElseIf node.Function Is BuiltinFunctions.Sqr Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Sqrt(value)
      ElseIf node.Function Is BuiltinFunctions.Str Then
        Dim value = EvaluateExpression(node.Arguments(0))
        Return Microsoft.VisualBasic.Str(value)
      ElseIf node.Function Is BuiltinFunctions.StringFunction Then
        Dim length = CInt(EvaluateExpression(node.Arguments(0)))
        Dim thing = EvaluateExpression(node.Arguments(1))
        If TypeOf thing Is String Then
          Return New String(CStr(thing)(0), length)
        Else
          Return New String(ChrW(CInt(thing)), length)
        End If
      ElseIf node.Function Is BuiltinFunctions.Tan Then
        Dim value = CDbl(EvaluateExpression(node.Arguments(0)))
        Return Math.Tan(value)
      ElseIf node.Function Is BuiltinFunctions.UBound Then
        Stop
        Return Nothing
      ElseIf node.Function Is BuiltinFunctions.UCase Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return value?.ToUpper
      ElseIf node.Function Is BuiltinFunctions.Val Then
        Dim value = CStr(EvaluateExpression(node.Arguments(0)))
        Return Microsoft.VisualBasic.Val(value)
      Else
        Dim locals = New Dictionary(Of VariableSymbol, Object)
        For i = 0 To node.Arguments.Length - 1
          Dim parameter = node.Function.Parameters(i)
          Dim value = EvaluateExpression(node.Arguments(i))
          Debug.Assert(value IsNot Nothing)
          locals.Add(parameter, value)
        Next
        m_locals.Push(locals)
        Dim statement = m_functions(node.Function)
        Dim result = EvaluateStatement(statement)
        m_locals.Pop()
        Return result
      End If
    End Function

    Private Function EvaluateConversionExpression(node As BoundConversionExpression) As Object
      Dim value = EvaluateExpression(node.Expression)
      If node.Type Is TypeSymbol.Any Then
        Return value
      ElseIf node.Type Is TypeSymbol.Boolean Then
        Return Convert.ToBoolean(value)
      ElseIf node.Type Is TypeSymbol.Byte Then
        Return Convert.ToByte(value)
      ElseIf node.Type Is TypeSymbol.SByte Then
        Return Convert.ToSByte(value)
      ElseIf node.Type Is TypeSymbol.Integer Then
        Return Convert.ToInt16(value)
      ElseIf node.Type Is TypeSymbol.UInteger Then
        Return Convert.ToUInt16(value)
      ElseIf node.Type Is TypeSymbol.Long Then
        Return Convert.ToInt32(value)
      ElseIf node.Type Is TypeSymbol.ULong Then
        Return Convert.ToUInt32(value)
      ElseIf node.Type Is TypeSymbol.Long64 Then
        Return Convert.ToInt64(value)
      ElseIf node.Type Is TypeSymbol.ULong64 Then
        Return Convert.ToUInt64(value)
      ElseIf node.Type Is TypeSymbol.Single Then
        Return Convert.ToSingle(value)
      ElseIf node.Type Is TypeSymbol.Double Then
        Return Convert.ToDouble(value)
      ElseIf node.Type Is TypeSymbol.String Then
        Return Convert.ToString(value)
      Else
        Throw New Exception($"Unexpected type {node.Type}")
      End If
    End Function

    Private Sub Assign(variable As VariableSymbol, value As Object)
      If variable.Kind = SymbolKind.GlobalVariable Then
        m_globals(variable) = value
      Else
        Dim locals = m_locals.Peek
        locals(variable) = value
      End If
    End Sub

  End Class

  '' This Evaluator walks the "raw" SyntaxTree.
  'Public NotInheritable Class Evaluator_SyntaxTree

  '  Private ReadOnly m_root As ExpressionSyntax

  '  Sub New(root As ExpressionSyntax)
  '    m_root = root
  '  End Sub

  '  Public Function Evaluate() As Integer
  '    Return EvaluateExpression(m_root)
  '  End Function

  '  Private Function EvaluateExpression(node As ExpressionSyntax) As Integer

  '    If TypeOf node Is LiteralExpressionSyntax Then
  '      Return CInt(CType(node, LiteralExpressionSyntax).LiteralToken.Value)
  '    End If

  '    If TypeOf node Is UnaryExpressionSyntax Then
  '      Dim u = CType(node, UnaryExpressionSyntax)
  '      Dim operand = EvaluateExpression(u.Operand)
  '      Select Case u.OperatorToken.Kind
  '        Case SyntaxKind.PlusToken : Return operand
  '        Case SyntaxKind.MinusToken : Return -operand
  '        Case Else
  '          Throw New Exception($"Unexpected unary operator {u.OperatorToken.Kind}")
  '      End Select
  '    End If

  '    If TypeOf node Is BinaryExpressionSyntax Then
  '      Dim b = CType(node, BinaryExpressionSyntax)
  '      Dim left = EvaluateExpression(b.Left)
  '      Dim right = EvaluateExpression(b.Right)
  '      Select Case b.OperatorToken.Kind
  '        Case SyntaxKind.PlusToken : Return left + right
  '        Case SyntaxKind.MinusToken : Return left - right
  '        Case SyntaxKind.StarToken : Return left * right
  '        Case SyntaxKind.SlashToken : Return CInt(left / right)
  '        Case SyntaxKind.BackslashToken : Return left \ right
  '        Case Else
  '          Throw New Exception($"Unexpected binary operator {b.OperatorToken.Kind}")
  '      End Select
  '    End If

  '    If TypeOf node Is ParenExpressionSyntax Then
  '      Dim p = CType(node, ParenExpressionSyntax)
  '      Return EvaluateExpression(p.Expression)
  '    End If

  '    Throw New Exception($"Unexpected node {node.Kind}")

  '  End Function

  'End Class

End Namespace
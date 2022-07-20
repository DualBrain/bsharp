Imports System.Collections.Immutable
Imports System.Text
Imports Bsharp.CodeAnalysis.Binding
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis.Emit

  ' This emitter is designed to handle Visual Basic.

  Friend NotInheritable Class JsEmitter

    Private ReadOnly _diagnostics As New DiagnosticBag

    Private m_moduleName As String
    Private m_contents As String = ""

    Friend Sub New(moduleName As String, references() As String)

      m_moduleName = moduleName

    End Sub

    Private m_tabs As Integer = 0

    Private Function Tab() As String
      Return Microsoft.VisualBasic.Space(m_tabs * 2)
    End Function

    Public Function Emit(program As BoundProgram, outputPath As String) As ImmutableArray(Of Diagnostic)

      If _diagnostics.Any Then Return _diagnostics.ToImmutableArray

      Dim template = <xml><![CDATA[
<html>
  <head>

    <script type="text/javascript">

      var terminateProgram = false;
      var curLine = null;
      var inkey = '';
      var cancelKeyEffects = true;

      function Console(elementId, rows, columns, promptWindowUrl) {
        this.element = document.getElementById(elementId);
        if (!this.element) {
          alert('No element with the ID ' + elementId + ' was found.');
          return; }
        while (this.element.hasChildNodes()) {
          this.element.removeChild(this.element.childNodes[0]);
        }
        this.element.style.whiteSpace = 'pre';
        this.rows = Math.floor(rows);
        this.columns = Math.floor(columns);
        this.cursorPosition = { row: 0, column: 0 };
        this.charGrid = new Array(this.rows);
        this.promptWindowUrl = promptWindowUrl;
        for (var i = 0; i < rows; i++) {
          var textNode = document.createTextNode('');
          this.charGrid[i] = textNode;
          this.element.appendChild(textNode);
          if (i < rows - 1) {
            this.element.appendChild(document.createElement('br')); }
        }
        this.cls();
      }

      Console.prototype.cls = function() {
        for (var row = 0; row < this.rows; row++) {
          var textNode = this.charGrid[row];
          var s = '';
          for (var col = 0; col < this.columns; col++) {
            s += ' '; 
          }
          textNode.data = s;
        }
        this.setCursorPos(0, 0);
      };

      Console.prototype.printAt = function(row, column, str, cycle) {
        if (row >= this.rows || row < 0 || column < 0 || !str) {
          return; 
        }
        var oldRow = this.charGrid[row].data;
        var newRow = oldRow.substring(0, column) + str;
        if (newRow.length < this.columns) {
          newRow += oldRow.substring(column + str.length);
          this.setCursorPos(row, column + str.length);
        } else {
          this.setCursorPos(row + 1, 0);
          if (cycle && this.cursorPosition.row >= this.rows) {
            for (var rowIndex = 0; rowIndex < this.rows - 1; rowIndex++) {
              this.charGrid[rowIndex].data = this.charGrid[rowIndex+1].data;
            }
            var emptyRow = '';
            for (var col = 0; col < this.columns; col++) {
              emptyRow += ' ';
            }
            this.charGrid[this.rows-1].data = emptyRow;
            this.cursorPosition.row--;
            row--;
          }
        }
        if (newRow.length > this.columns) {
          newRow = newRow.substring(0, this.columns);
        }
        this.charGrid[row].data = newRow;
      };

      Console.prototype.print = function(str) {
        if (!str) { str = ''; }
        str = '' + str;
        var newColumnPos = this.cursorPosition.column + str.length;
        if (newColumnPos > this.columns) {
          var charsLeftOnCurLine = this.columns - this.cursorPosition.column;
          var s = str.substring(0, charsLeftOnCurLine);
          this.print(s);
          this.print(str.substring(charsLeftOnCurLine));
        } else {
          this.printAt(this.cursorPosition.row, this.cursorPosition.column, str, true);
        }
      };

      Console.prototype.println = function(str) {
        if (!str) { str = ''; }
        this.print(str);
        var extraChars = this.charGrid[this.cursorPosition.row].data.substring(this.cursorPosition.column);
        this.print(extraChars);
      };

      Console.prototype.setCursorPos = function(row, column) {
        this.cursorPosition.row = row;
        this.cursorPosition.column = column;
      };

      Console.prototype.input = function(message) {
        if (message) { this.print(message); }
        var result;
        if (window.showModalDialog) {
          if (!this.promptWindowUrl) {
            alert('JS Console Error\nConsole.promptWindowUrl not set. Set this to the URL of PromptWindow.htm\nPrompts disabled in Internet Explorer.');
            return '';
          }
          result = window.showModalDialog(this.promptWindowUrl, message, "dialogWidth:300px;dialogHeight:200px");
        } else {
          result = prompt(message);
        }
        if (result) {
          this.println(result);
          return result;
        } else {
          return '';
        }
      };

      function getInkey() {
	      var c = this.inkey;
	      inkey = '';
	      return c;
      }

      function setInkey(e) {
        e = (e ? e : window.event);
        var code = (e.keyCode ? e.keyCode : e.charCode);
        if (code) {
          var c = String.fromCharCode(code);
          inkey = c;
          if (cancelKeyEffects) {
            if (e.cancelable) {
              e.preventDefault();
              e.stopPropagation();
            } else {
              e.returnValue = false;
              e.cancelBubble = true;
            }
          }
        }
      }

      function generateString(numToGet, character) {
        var s = '';
        var c = character.toString().charAt(0);
        for (var i = 0; i < numToGet; i++) {
          s += c;
        }
        return s;
      }

      function getSpaces(numToGet) {
        return generateString(numToGet, ' ');
      }

      function run(consoleContainerId, consoleRows, consoleColumns, firstLine) {
        console = new Console(consoleContainerId, consoleRows, consoleColumns, "PromptWindow.htm");
        if (document.addEventListener){
          document.addEventListener('keypress', setInkey, false); 
        } else if (document.attachEvent){
          document.attachEvent('onkeypress', setInkey);
        } else {
          document.onkeydown = setInkey;
        }
        curLine = firstLine;
        mainLoop();
      }

      function mainLoop() {
        if (terminateProgram || curLine == null) {
          return;
        }
        try {
          curLine = curLine();
        } catch (e) {
          if (e == 'ProgramAbortException') {
            console.println();
            console.println("Program execution stopped.");
            return;
          } else {
            throw e;
          }
        }
        setTimeout('mainLoop()', 10);
      }

      function terminate() {
        terminateProgram = true;
      }

    </script>
    <style type="text/css">
      .dos {
        font-family: Courier New, Courier;
        background-color: Black;
        color: White;
        display:block;
        float:left;
        padding: 2px; }
      .commodore64 {
        font-family: Courier;
        background-color: #4242E7;
        color: #A5A5FF;
        display:block;
        float:left;
        font-weight: bold;
        text-transform:uppercase;
        font-size:17px;
        border: solid 20px #A5A5FF;
        padding: 2px; }
      </style>
    </head>
    <body>
    <div id="console" class="dos"></div>
  </body>
  <script type="text/javascript">

    var console = new Console('console', 25, 80);

]]></xml>.Value

      m_contents = template

      m_tabs += 2

      For Each f In program.Functions
        EmitFunctionDeclaration(f.Key, f.Value)
        m_contents &= $"{vbCrLf}"
      Next

      m_contents &= $"{Tab()}main();{vbCrLf}"

      m_contents &= "
  </script>
</html>"

      System.IO.File.WriteAllText(outputPath, m_contents)

      Return _diagnostics.ToImmutableArray

    End Function

    Private Sub EmitFunctionDeclaration(func As FunctionSymbol, body As BoundBlockStatement)
      Dim functionType = TypeSymbol.TypeSymbolToType(func.Type)
      Dim functionName = func.Name
      m_contents &= $"{Tab()}function {functionName}("
      For Each parameter In func.Parameters
        Dim parameterType = parameter.Type
        Dim parameterName = parameter.Name
        m_contents &= $"{parameterType} {parameterName}, "
      Next
      m_contents &= $") {{{vbCrLf}"
      m_tabs += 1
      EmitFunctionBody(func, body)
      m_tabs -= 1
      m_contents &= $"{Tab()}}}{vbCrLf}"
    End Sub

    Private Sub EmitFunctionBody(func As FunctionSymbol, body As BoundBlockStatement)
      For Each statement In body.Statements
        EmitStatement(statement)
      Next
    End Sub

    Private Sub EmitStatement(node As BoundStatement)
      Select Case node.Kind
        'Case BoundNodeKind.ConditionalGotoStatement : EmitConditionalGotoStatement(ilProcessor, CType(node, BoundConditionalGotoStatement))
        'Case BoundNodeKind.GotoStatement : EmitGotoStatement(CType(node, BoundGotoStatement))
        'Case BoundNodeKind.ExpressionStatement : EmitExpressionStatement(ilProcessor, CType(node, BoundExpressionStatement))
        'Case BoundNodeKind.HandleCommaStatement : EmitHandleCommaStatement(ilProcessor, CType(node, BoundHandleCommaStatement))
        Case BoundNodeKind.HandlePrintLineStatement : EmitHandlePrintLineStatement(CType(node, BoundHandlePrintLineStatement))
        Case BoundNodeKind.HandlePrintStatement : EmitHandlePrintStatement(CType(node, BoundHandlePrintStatement))
          'Case BoundNodeKind.HandleSpcStatement : EmitHandleSpcStatement(ilProcessor, CType(node, BoundHandleSpcStatement))
          'Case BoundNodeKind.HandleTabStatement : EmitHandleTabStatement(ilProcessor, CType(node, BoundHandleTabStatement))
          'Case BoundNodeKind.LabelStatement : EmitLabelStatement(CType(node, BoundLabelStatement))
          'Case BoundNodeKind.NopStatement : EmitNopStatement(ilProcessor, CType(node, BoundNopStatement))
          'Case BoundNodeKind.RemStatement : EmitRemStatement(ilProcessor, CType(node, BoundRemStatement))
          'Case BoundNodeKind.PrintStatement : EmitPrintStatement(CType(node, BoundPrintStatement))
        Case BoundNodeKind.LetStatement : EmitLetStatement(CType(node, BoundLetStatement))
        Case BoundNodeKind.ReturnStatement : EmitReturnStatement(CType(node, BoundReturnStatement))
          'Case BoundNodeKind.VariableDeclaration : EmitVariableDeclaration(ilProcessor, CType(node, BoundVariableDeclaration))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    Private Sub EmitAssignmentExpression(node As BoundAssignmentExpression)
      'Dim variableDefinition = _locals(node.Variable)
      'EmitExpression(ilProcessor, node.Expression)
      'ilProcessor.Emit(OpCodes.Dup)
      'ilProcessor.Emit(OpCodes.Stloc, variableDefinition)
      Stop
    End Sub

    Private Sub EmitBinaryExpression(node As BoundBinaryExpression)

      If node.Op.Kind = BoundBinaryOperatorKind.IntegerDivision Then
        m_contents &= "parseInt("
        EmitExpression(node.Left)
        m_contents &= "/"
        EmitExpression(node.Right)
        m_contents &= ")"
        Return
      End If

      ' +(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.Addition Then
        If node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String Then
          EmitStringConcatExpression(node)
          Return
        End If
      End If

      EmitExpression(node.Left)

      ' ==(any, any)
      ' ==(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.Equal Then
        If (node.Left.Type Is TypeSymbol.Any AndAlso node.Right.Type Is TypeSymbol.Any) OrElse
           (node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String) Then
          'ilProcessor.Emit(OpCodes.Call, _objectEqualsReference)
          Stop
          Return
        End If
      End If

      ' !=(any, any)
      ' !=(string, string)
      If node.Op.Kind = BoundBinaryOperatorKind.NotEqual Then
        If (node.Left.Type Is TypeSymbol.Any AndAlso node.Right.Type Is TypeSymbol.Any) OrElse
           (node.Left.Type Is TypeSymbol.String AndAlso node.Right.Type Is TypeSymbol.String) Then
          'ilProcessor.Emit(OpCodes.Call, _objectEqualsReference)
          'ilProcessor.Emit(OpCodes.Ldc_I4_0)
          'ilProcessor.Emit(OpCodes.Ceq)
          Stop
          Return
        End If
      End If

      Select Case node.Op.Kind
        Case BoundBinaryOperatorKind.Addition
          m_contents &= "+"
        Case BoundBinaryOperatorKind.Subtraction
          m_contents &= "-"
        Case BoundBinaryOperatorKind.Multiplication
          m_contents &= "*"
        Case BoundBinaryOperatorKind.Division
          m_contents &= "/"
        'Case BoundBinaryOperatorKind.IntegerDivision ' Handled above...
        '  m_contents &= "\"
        Case BoundBinaryOperatorKind.LogicalAnd
          m_contents &= "&&"
        Case BoundBinaryOperatorKind.LogicalOr
          m_contents &= "||"
        Case BoundBinaryOperatorKind.BitwiseAnd
          m_contents &= "&"
        Case BoundBinaryOperatorKind.BitwiseOr
          m_contents &= "|"
        Case BoundBinaryOperatorKind.BitwiseXor
          m_contents &= "^"
        Case BoundBinaryOperatorKind.Equal
          m_contents &= "=="
        Case BoundBinaryOperatorKind.NotEqual
          m_contents &= "!="
        Case BoundBinaryOperatorKind.LessThan
          m_contents &= "<"
        Case BoundBinaryOperatorKind.LessThanEqual
          m_contents &= "<="
        Case BoundBinaryOperatorKind.GreaterThan
          m_contents &= ">"
        Case BoundBinaryOperatorKind.GreaterThanEqual
          m_contents &= ">="
        Case Else
          Throw New Exception($"Unexpected binary operator {SyntaxFacts.GetText(node.Op.SyntaxKind)}({node.Op.Type})")
      End Select

      EmitExpression(node.Right)

    End Sub

    Private Sub EmitCallExpression(node As BoundCallExpression)

      Stop

      'If node.Function Is BuiltinFunctions.Rnd2 Then

      '  If _randomFieldDefinition Is Nothing Then
      '    EmitRandomField()
      '  End If

      '  ilProcessor.Emit(OpCodes.Ldsfld, _randomFieldDefinition)
      '  For Each argument In node.Arguments
      '    EmitExpression(ilProcessor, argument)
      '  Next
      '  ilProcessor.Emit(OpCodes.Callvirt, _randomNextReference)

      '  Return

      'End If

      'For Each argument In node.Arguments
      '  EmitExpression(ilProcessor, argument)
      'Next

      ''If node.Function Is Input Then
      ''  ilProcessor.Emit(OpCodes.Call, _consoleReadLineReference)
      ''  'ElseIf node.Function Is Print Then
      ''  '  ilProcessor.Emit(OpCodes.Call, _consoleWriteLineReference)
      ''Else
      'Dim methodDefinition = _methods(node.Function)
      'ilProcessor.Emit(OpCodes.Call, methodDefinition)
      ''End If

    End Sub

    'Private Sub EmitConditionalGotoStatement(ilProcessor As ILProcessor, node As BoundConditionalGotoStatement)
    '  EmitExpression(ilProcessor, node.Condition)
    '  Dim opCode = If(node.JumpIfTrue, OpCodes.Brtrue, OpCodes.Brfalse)
    '  _fixups.Add((ilProcessor.Body.Instructions.Count, node.Label))
    '  ilProcessor.Emit(opCode, Instruction.Create(OpCodes.Nop))
    'End Sub

    Private Sub EmitConstantExpression(node As BoundExpression)
      Debug.Assert(node.ConstantValue IsNot Nothing)
      If node.Type Is TypeSymbol.Boolean Then
        Dim value = CBool(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.Long64 Then
        Dim value = CLng(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.Integer Then
        Dim value = CShort(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.Long Then
        Dim value = CInt(node.ConstantValue.Value)
        m_contents &= $"{value}"
      ElseIf node.Type Is TypeSymbol.String Then
        Dim value = CStr(node.ConstantValue.Value)
        m_contents &= $"'{value.Replace("\", "\\")}'"
      Else
        Throw New Exception($"Unexpected constant expression type: {node.Type}")
      End If
    End Sub

    Private Sub EmitConversionExpression(node As BoundConversionExpression)
      'Dim needsBoxing = node.Expression.Type Is TypeSymbol.Boolean OrElse
      '                  node.Expression.Type Is TypeSymbol.Integer
      'If needsBoxing Then ilProcessor.Emit(OpCodes.Box, _knownTypes(node.Expression.Type))
      If node.Type Is TypeSymbol.Any Then
        ' Done.
      ElseIf node.Type Is TypeSymbol.Boolean Then
        'm_contents &= "(bool)"
      ElseIf node.Type Is TypeSymbol.Integer Then
        'm_contents &= "(int)"
      ElseIf node.Type Is TypeSymbol.double Then
        'm_contents &= "(float)"
      ElseIf node.Type Is TypeSymbol.String Then
        'm_contents &= "(string)"
      Else
        Throw New Exception($"Unexpected conversion from {node.Expression.Type} to {node.Type}")
      End If
      EmitExpression(node.Expression)
    End Sub

    Private Sub EmitExpression(node As BoundExpression)

      If node.ConstantValue IsNot Nothing Then
        EmitConstantExpression(node)
        Return
      End If

      Select Case node.Kind
        Case BoundNodeKind.VariableExpression : EmitVariableExpression(CType(node, BoundVariableExpression))
        Case BoundNodeKind.AssignmentExpression : EmitAssignmentExpression(CType(node, BoundAssignmentExpression))
        Case BoundNodeKind.UnaryExpression : EmitUnaryExpression(CType(node, BoundUnaryExpression))
        Case BoundNodeKind.BinaryExpression : EmitBinaryExpression(CType(node, BoundBinaryExpression))
        Case BoundNodeKind.CallExpression : EmitCallExpression(CType(node, BoundCallExpression))
        Case BoundNodeKind.ConversionExpression : EmitConversionExpression(CType(node, BoundConversionExpression))
        Case Else
          Throw New Exception($"Unexpected node kind {node.Kind}")
      End Select
    End Sub

    'Private Sub EmitExpressionStatement(ilProcessor As ILProcessor, node As BoundExpressionStatement)
    '  EmitExpression(ilProcessor, node.Expression)
    '  If node.Expression.Type IsNot TypeSymbol.Nothing Then
    '    ilProcessor.Emit(OpCodes.Pop)
    '  End If
    'End Sub

    Private Sub EmitGotoStatement(node As BoundGotoStatement)
      Dim label = node.Label.Name
      If label.StartsWith("$") Then
        label = label.Substring(1)
      End If
      m_contents &= $"{Tab()}Goto {label}{vbCrLf}"
    End Sub

    'Private Shared Sub EmitHandleCommaStatement(ilProcessor As ILProcessor, node As BoundHandleCommaStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    Private Sub EmitHandlePrintLineStatement(node As BoundHandlePrintLineStatement)
      m_contents &= $"{Tab()}console.println();{vbCrLf}"
    End Sub

    Private Sub EmitHandlePrintStatement(node As BoundHandlePrintStatement)
      m_contents &= $"{Tab()}console.print("
      EmitExpression(node.Expression)
      m_contents &= $");{vbCrLf}"
    End Sub

    'Private Shared Sub EmitHandleSpcStatement(ilProcessor As ILProcessor, node As BoundHandleSpcStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    'Private Shared Sub EmitHandleTabStatement(ilProcessor As ILProcessor, node As BoundHandleTabStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    Private Sub EmitLabelStatement(node As BoundLabelStatement)
      Dim label = node.Label.Name
      If label.StartsWith("$") Then
        label = label.Substring(1)
      End If
      m_contents &= $"{label}:{vbCrLf}"
    End Sub

    'Private Shared Sub EmitNopStatement(ilProcessor As ILProcessor, node As BoundNopStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    'Private Shared Sub EmitRemStatement(ilProcessor As ILProcessor, node As BoundRemStatement)
    '  If node Is Nothing Then
    '  End If
    '  ilProcessor.Emit(OpCodes.Nop)
    'End Sub

    'Private Sub EmitPrintStatement(node As BoundPrintStatement)
    '  m_contents &= $"{Tab()}' PRINT ????{vbCrLf}"
    'End Sub

    'Private Sub EmitRandomField()
    '  _randomFieldDefinition = New FieldDefinition("$rnd", Ccl.FieldAttributes.Static Or
    '                                                       Ccl.FieldAttributes.Private, _randomReference)
    '  _typeDefinition.Fields.Add(_randomFieldDefinition)
    '  Dim staticConstructor = New Ccl.MethodDefinition(".cctor", Ccl.MethodAttributes.Static Or
    '                                                             Ccl.MethodAttributes.Private Or
    '                                                             Ccl.MethodAttributes.SpecialName Or
    '                                                             Ccl.MethodAttributes.RTSpecialName, _knownTypes(TypeSymbol.Nothing))
    '  _typeDefinition.Methods.Insert(0, staticConstructor)
    '  Dim ilProcessor = staticConstructor.Body.GetILProcessor
    '  ilProcessor.Emit(OpCodes.Newobj, _randomCtorReference)
    '  ilProcessor.Emit(OpCodes.Stsfld, _randomFieldDefinition)
    '  ilProcessor.Emit(OpCodes.Ret)
    'End Sub

    Private Sub EmitLetStatement(node As BoundLetStatement)
      m_contents &= $"{Tab()}var {node.Variable.Name} = "
      EmitExpression(node.Expression)
      m_contents &= $";{vbCrLf}"
    End Sub

    Private Sub EmitReturnStatement(node As BoundReturnStatement)
      m_contents &= $"{Tab()}return"
      If node.Expression IsNot Nothing Then m_contents &= " " : EmitExpression(node.Expression)
      m_contents &= $";{vbCrLf}"
    End Sub

    Private Sub EmitStringConcatExpression(node As BoundBinaryExpression)

      ' Flatten the expression tree to a sequence of nodes to concatenate, then fold consecutive constants in that sequence.
      ' This approach enables constant folding of non-sibling nodes, which cannot be done in the ConstantFolding class as it would require changing the tree.
      ' Example: folding b And c in ((a + b) + c) if they are constant.

      Dim nodes = EmitStringConcatExpression_FoldConstants(EmitStringConcatExpression_Flatten(node)).ToList()

      Select Case nodes.Count
        Case 0
          m_contents &= "''"

        Case 1
          EmitExpression(nodes(0))

        Case 2
          EmitExpression(nodes(0))
          EmitExpression(nodes(1))

        Case 3
          EmitExpression(nodes(0))
          EmitExpression(nodes(1))
          EmitExpression(nodes(2))

        Case 4
          EmitExpression(nodes(0))
          EmitExpression(nodes(1))
          EmitExpression(nodes(2))
          EmitExpression(nodes(3))

        Case Else
          Stop
          '_ilProcessor.Emit(OpCodes.Ldc_I4, nodes.Count)
          '_ilProcessor.Emit(OpCodes.Newarr, _knownTypes(TypeSymbol.[String]))
          'For i = 0 To nodes.Count - 1
          '  _ilProcessor.Emit(OpCodes.Dup)
          '  _ilProcessor.Emit(OpCodes.Ldc_I4, i)
          '  EmitExpression(_ilProcessor, nodes(i))
          '  _ilProcessor.Emit(OpCodes.Stelem_Ref)
          'Next
          '_ilProcessor.Emit(OpCodes.[Call], _stringConcatArrayReference)

      End Select

    End Sub

    Private Iterator Function EmitStringConcatExpression_Flatten(node As BoundExpression) As IEnumerable(Of BoundExpression)

      Dim binaryExpression = TryCast(node, BoundBinaryExpression)
      If binaryExpression IsNot Nothing AndAlso
         binaryExpression.Op.Kind = BoundBinaryOperatorKind.Addition AndAlso
         binaryExpression.Left.Type Is TypeSymbol.[String] AndAlso
         binaryExpression.Right.Type Is TypeSymbol.[String] Then

        For Each result In EmitStringConcatExpression_Flatten(binaryExpression.Left)
          Yield result
        Next

        For Each result In EmitStringConcatExpression_Flatten(binaryExpression.Right)
          Yield result
        Next

      Else

        If node.Type IsNot TypeSymbol.[String] Then
          Throw New Exception($"Unexpected node type in string concatenation: {node.Type}")
        End If

        Yield node

      End If

    End Function

    Private Shared Iterator Function EmitStringConcatExpression_FoldConstants(nodes As IEnumerable(Of BoundExpression)) As IEnumerable(Of BoundExpression)

      Dim sb As StringBuilder = Nothing

      For Each node In nodes
        If node.ConstantValue IsNot Nothing Then
          Dim stringValue = CStr(node.ConstantValue.Value)

          If String.IsNullOrEmpty(stringValue) Then
            Continue For
          End If

          sb = If(sb, New StringBuilder)
          sb.Append(stringValue)

        Else

          If sb?.Length > 0 Then
            Yield New BoundLiteralExpression(sb.ToString())
            sb.Clear()
          End If

          Yield node

        End If
      Next

      If sb?.Length > 0 Then
        Yield New BoundLiteralExpression(sb.ToString())
      End If

    End Function

    Private Sub EmitUnaryExpression(node As BoundUnaryExpression)
      'EmitExpression(ilProcessor, node.Operand)
      'If node.Op.Kind = BoundUnaryOperatorKind.Identity Then
      '  ' Done.
      'ElseIf node.Op.Kind = BoundUnaryOperatorKind.LogicalNegation Then
      '  ilProcessor.Emit(OpCodes.Ldc_I4_0)
      '  ilProcessor.Emit(OpCodes.Ceq)
      'ElseIf node.Op.Kind = BoundUnaryOperatorKind.Negation Then
      '  ilProcessor.Emit(OpCodes.Neg)
      'ElseIf node.Op.Kind = BoundUnaryOperatorKind.BitwiseComplement Then
      '  ilProcessor.Emit(OpCodes.Not)
      'Else
      '  Throw New Exception($"Unexpected unary operator {SyntaxFacts.GetText(node.Op.SyntaxKind)}({node.Operand.Type})")
      'End If
      Stop
    End Sub

    'Private Sub EmitVariableDeclaration(ilProcessor As ILProcessor, node As BoundVariableDeclaration)
    '  Dim typeReference = _knownTypes(node.Variable.Type)
    '  Dim variableDefinition = New VariableDefinition(typeReference)
    '  _locals.Add(node.Variable, variableDefinition)
    '  ilProcessor.Body.Variables.Add(variableDefinition)
    '  EmitExpression(ilProcessor, node.Initializer)
    '  ilProcessor.Emit(OpCodes.Stloc, variableDefinition)
    'End Sub

    Private Sub EmitVariableExpression(node As BoundVariableExpression)
      If TypeOf node.Variable Is ParameterSymbol Then
        Dim parameter = CType(node.Variable, ParameterSymbol)
        'ilProcessor.Emit(OpCodes.Ldarg, parameter.Ordinal)
        m_contents &= $"{parameter}"
      Else
        'Dim variableDefinition = _locals(node.Variable)
        'ilProcessor.Emit(OpCodes.Ldloc, variableDefinition)
        m_contents &= $"{node.Variable.Name}"
      End If
    End Sub

#Region "Converted from 'inline' functions."

    Private Function ProperCase(value As String) As String
      If "abcdefghijklmnopqrstuvwxyz".Contains(value(0)) Then
        Return value(0).ToString.ToUpper & value.Substring(1)
      Else
        Return value
      End If
    End Function

    'Private Function Emit_ResolveType(assemblies As List(Of AssemblyDefinition),
    '                                  internalName As String,
    '                                  metadataName As String) As TypeReference
    '  Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
    '                              SelectMany(Function(m) m.Types).
    '                              Where(Function(t) t.FullName = metadataName).ToArray
    '  If foundTypes.Length = 1 Then
    '    Dim typeReference = _assemblyDefinition.MainModule.ImportReference(foundTypes(0))
    '    Return typeReference
    '  ElseIf foundTypes.Length = 0 Then
    '    _diagnostics.ReportRequiredTypeNotFound(internalName, metadataName)
    '  Else
    '    _diagnostics.ReportRequiredTypeAmbiguous(internalName, metadataName, foundTypes)
    '  End If
    '  Return Nothing
    'End Function

    'Private Function Emit_ResolveMethod(assemblies As List(Of AssemblyDefinition),
    '                                    typeName As String,
    '                                    methodName As String,
    '                                    parameterTypeNames As String()) As MethodReference
    '  Dim foundTypes = assemblies.SelectMany(Function(a) a.Modules).
    '                              SelectMany(Function(m) m.Types).
    '                              Where(Function(t) t.FullName = typeName).ToArray
    '  If foundTypes.Length = 1 Then
    '    Dim foundType = foundTypes(0)
    '    Dim methods = foundType.Methods.Where(Function(m) m.Name = methodName)

    '    For Each method In methods
    '      If method.Parameters.Count <> parameterTypeNames.Length Then
    '        Continue For
    '      End If
    '      Dim allParametersMatch = True
    '      For i = 0 To parameterTypeNames.Length - 1
    '        If method.Parameters(i).ParameterType.FullName <> parameterTypeNames(i) Then
    '          allParametersMatch = False
    '          Exit For
    '        End If
    '      Next
    '      If Not allParametersMatch Then
    '        Continue For
    '      End If
    '      Return _assemblyDefinition.MainModule.ImportReference(method)
    '    Next
    '    _diagnostics.ReportRequiredMethodNotFound(typeName, methodName, parameterTypeNames)
    '    Return Nothing
    '  ElseIf foundTypes.Length = 0 Then
    '    _diagnostics.ReportRequiredTypeNotFound(Nothing, typeName)
    '  Else
    '    _diagnostics.ReportRequiredTypeAmbiguous(Nothing, typeName, foundTypes)
    '  End If
    '  Return Nothing
    'End Function

#End Region

  End Class

End Namespace
Option Explicit On
Option Strict On
Option Infer On

Imports Bsharp.CodeAnalysis
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax
Imports Bsharp.CodeAnalysis.Authoring
Imports Bsharp.IO
Imports System.IO

Namespace Bsharp

  Friend NotInheritable Class BsharpRepl
    Inherits Repl

    'Private Shared ReadOnly m_loadingSubmission As Boolean
    Private Shared ReadOnly m_emptyCompilation As Compilation = Compilation.CreateScript(Nothing)
    Private m_previous As Compilation = Nothing
    Private m_showTree As Boolean = False
    Private m_showProgram As Boolean = False
    Private ReadOnly m_variables As New Dictionary(Of VariableSymbol, Object)

    Sub New()

      MyBase.m_immediateCommands = New List(Of String) From {"edit",
                                                             "files",
                                                             "list",
                                                             "load",
                                                             "new",
                                                             "quit",
                                                             "run",
                                                             "save"}

      Console.WriteLine("BASIC .NET (""B#"") ver 0.0.1-alpha.3")
      Console.WriteLine("(C) Copyright 2010-2022 Cory Smith")
      Console.WriteLine()

      'LoadSubmissions()

    End Sub

    Protected Overrides Function RenderLine(lines As IReadOnlyList(Of String), lineIndex As Integer, state As Object) As Object

      Dim tree As SyntaxTree

      Dim text As String = Nothing
      If state Is Nothing Then
        text = String.Join(Environment.NewLine, lines)
        tree = SyntaxTree.Parse(text)
      Else
        tree = CType(state, SyntaxTree)
      End If

      Dim line = tree.Text.Lines(lineIndex)
      Dim lineSpan = line.Span
      Dim classifiedSpans = Classifier.Classify(tree, lineSpan)

      For Each classifiedSpan In classifiedSpans

        Dim classifiedText = tree.Text.ToString(classifiedSpan.Span)

        'Console.BackgroundColor = ConsoleColor.DarkGray
        Select Case classifiedSpan.Classification
          Case Classification.Text : Console.ForegroundColor = ConsoleColor.White ': Console.BackgroundColor = ConsoleColor.Green
          Case Classification.Keyword : Console.ForegroundColor = ConsoleColor.Blue
          Case Classification.Identifier : Console.ForegroundColor = ConsoleColor.DarkYellow
          Case Classification.Number : Console.ForegroundColor = ConsoleColor.Cyan
          Case Classification.String : Console.ForegroundColor = ConsoleColor.Magenta
          Case Classification.Comment : Console.ForegroundColor = ConsoleColor.Green
          Case Else
            Console.ForegroundColor = ConsoleColor.White
        End Select

        If String.IsNullOrEmpty(classifiedText) Then
          Dim length = classifiedSpan.Span.Length
          If length = 0 Then
            length = 1
          End If
          classifiedText = text?.Substring(classifiedSpan.Span.Start, length)
          Console.ForegroundColor = ConsoleColor.Red
        End If

        Console.Write(classifiedText)
        Console.ResetColor()

      Next

      Return tree

    End Function

    <MetaCommand("edit", "Toggle 'edit' mode.")>
    Protected Sub EvaluateEdit()
      m_fullScreenEditor = True
    End Sub

    <MetaCommand("files", "List all files in currrent path.")>
    Protected Shared Sub EvaluateFiles()

      Dim files = System.IO.Directory.GetFiles(".", "*.*")
      For Each file In files
        Console.WriteLine(System.IO.Path.GetFileName(file))
      Next

    End Sub

    <MetaCommand("list", "List the 'edit' text.")>
    Protected Sub EvaluateList()
      Dim text = m_edit
      LoadDocument(text)
    End Sub

    <MetaCommand("listsymbols", "Lists symbols")>
    Protected Sub EvaluateListSymbols()

      'If m_previous Is Nothing Then
      '  Return
      'End If
      'Dim symbols = m_previous.GetSymbols.OrderBy(Function(s) s.Kind).ThenBy(Function(s) s.Name)
      Dim compilation = If(m_previous, m_emptyCompilation)
      Dim symbols = compilation.GetSymbols.OrderBy(Function(s) s.Kind).ThenBy(Function(s) s.Name)

      For Each symbol In symbols
        symbol.WriteTo(Console.Out)
        Console.WriteLine()
      Next

    End Sub

    <MetaCommand("load", "Loads a script file")>
    Protected Sub EvaluateLoad(path As String)

      path = System.IO.Path.GetFullPath(path)

      Dim ext = System.IO.Path.GetExtension(path)
      If String.IsNullOrWhiteSpace(ext) Then
        If System.IO.File.Exists(path & ".bs") Then
          path &= ".bs"
        ElseIf System.IO.File.Exists(path & ".bas") Then
          path &= ".bas"
        End If
      End If

      If Not System.IO.File.Exists(path) Then
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine($"error: file does not exist '{path}'")
        Console.ResetColor()
        Return
      End If

      m_edit = System.IO.File.ReadAllText(path)
      If Not String.IsNullOrEmpty(m_edit) Then
        m_edit &= vbCrLf
      End If
      m_fullScreenEditor = True

      'Dim text = System.IO.File.ReadAllText(path)
      'EvaluateSubmission(text)

    End Sub

    <MetaCommand("new", "Clears all previous submissions")>
    Protected Sub EvaluateReset()
      m_edit = ""
      m_previous = Nothing
      m_variables.Clear()
      'ClearSubmissions()
    End Sub

    <MetaCommand("quit", "Exits the REPL")>
    Protected Shared Sub EvaluateQuit()
      Environment.Exit(0)
    End Sub

    <MetaCommand("run", "Interpret 'edit' text.")>
    Protected Sub EvaluateRun()
      Dim text = m_edit
      EvaluateSubmission(text)
    End Sub

    <MetaCommand("save", "Save a script file")>
    Protected Sub EvaluateSave(path As String)

      path = System.IO.Path.GetFullPath(path)

      Dim ext = System.IO.Path.GetExtension(path)
      If String.IsNullOrEmpty(ext) Then
        If System.IO.File.Exists(path & ".bas") Then
          path &= ".bas"
        Else
          path &= ".bs"
        End If
      End If

      If System.IO.File.Exists(path) Then
        System.IO.File.Delete(path)
      End If

      System.IO.File.WriteAllText(path, m_edit)

    End Sub

    <MetaCommand("showfunctiontree", "Shows bound tree of a given function")>
    Protected Sub EvaluateShowFunction(functionName As String)

      'If m_previous Is Nothing Then
      '  Return
      'End If
      'Dim symbol = m_previous.GetSymbols.OfType(Of FunctionSymbol).SingleOrDefault(Function(f) f.Name = functionName)
      Dim compilation = If(m_previous, m_emptyCompilation)
      Dim symbol = compilation.GetSymbols.OfType(Of FunctionSymbol).SingleOrDefault(Function(f) f.Name = functionName.ToLower)

      If symbol Is Nothing Then
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine($"error: function '{functionName}' does not exist")
        Console.ResetColor()
        Return
      End If

      'm_previous.EmitTree(symbol, Console.Out)
      compilation.EmitTree(symbol, Console.Out)

    End Sub

    <MetaCommand("toggleboundtree", "Toggles the bound tree")>
    Protected Sub EvaluateShowProgram()
      m_showProgram = Not m_showProgram
      Console.WriteLine(If(m_showProgram, "Showing bound tree.", "Now showing bound tree."))
    End Sub

    <MetaCommand("toggleparsetree", "Toggles the parse tree")>
    Protected Sub EvaluateShowTree()
      m_showTree = Not m_showTree
      Console.WriteLine(If(m_showTree, "Showing parse trees.", "Now showing parse trees."))
    End Sub

    Protected Overrides Function IsCompleteSubmission(text As String) As Boolean

      If String.IsNullOrEmpty(text) Then Return True

      Dim lastTwoLinesAreBlank = text.Split(Environment.NewLine).
                                            Reverse().
                                            TakeWhile(Function(s) String.IsNullOrEmpty(s)).
                                            Take(2).
                                            Count() = 2

      If lastTwoLinesAreBlank Then Return True

      Dim tree = SyntaxTree.Parse(text)

      ' Use Members because we need to exclude the EndOfFileToken.
      Dim lastMember = tree.Root.Members.LastOrDefault
      If lastMember Is Nothing OrElse lastMember.GetLastToken.IsMissing Then
        Return False
      End If

      Return True

    End Function

    Protected Overrides Sub EvaluateSubmission(text As String)

      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = Bsharp.CodeAnalysis.Compilation.CreateScript(m_previous, tree)

      If m_showTree Then tree.Root.WriteTo(Console.Out)
      If m_showProgram Then compilation.EmitTree(Console.Out)

      Dim result = compilation.Evaluate(m_variables)
      Console.Out.WriteDiagnostics(result.Diagnostics)

      If Not result.Diagnostics.HasErrors Then

        If TypeOf result.Value Is UInt64 Then
          Environment.Exit(0)
        End If

        ' The expression evaluator will return a 
        ' result of some sort, the below code will
        ' output this final "top level" result.
        ' Now that we have `print`, shouldn't need.
        'If result.Value IsNot Nothing Then
        '  Console.ForegroundColor = ConsoleColor.White
        '  Console.WriteLine(result.Value)
        '  Console.ResetColor()
        'End If

        m_previous = compilation

        'SaveSubmission(text)

      End If

    End Sub

    'Private Shared Function GetSubmissionsDirectory() As String
    '  Dim localAppData = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
    '  Dim submissionsDirectory = Path.Combine(localAppData, "Bsharp", "Submissions")
    '  Return submissionsDirectory
    'End Function

    'Private Sub LoadSubmissions()

    '  Dim submissionsDirectory = GetSubmissionsDirectory()
    '  If Not Directory.Exists(submissionsDirectory) Then Return
    '  Dim files = Directory.GetFiles(submissionsDirectory).OrderBy(Function(f) f).ToArray
    '  If files.Length = 0 Then Return

    '  Console.ForegroundColor = ConsoleColor.DarkGray
    '  Console.WriteLine($"Loaded {files.Length} submission(s)")
    '  Console.ResetColor()

    '  m_loadingSubmission = True

    '  For Each file In files
    '    Dim text = System.IO.File.ReadAllText(file)
    '    EvaluateSubmission(text)
    '  Next

    '  m_loadingSubmission = False

    'End Sub

    'Private Sub ClearSubmissions()
    '  Dim dir = GetSubmissionsDirectory()
    '  If Directory.Exists(dir) Then
    '    Directory.Delete(dir, recursive:=True)
    '  End If
    'End Sub

    'Private Sub SaveSubmission(text As String)
    '  If m_loadingSubmission Then Return
    '  Dim submissionsDirectory = GetSubmissionsDirectory()
    '  Directory.CreateDirectory(submissionsDirectory)
    '  Dim count = Directory.GetFiles(submissionsDirectory).Length
    '  Dim name = $"submission{count:0000}"
    '  Dim fileName = Path.Combine(submissionsDirectory, name)
    '  File.WriteAllText(fileName, text)
    'End Sub

  End Class

End Namespace
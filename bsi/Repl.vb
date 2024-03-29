﻿Imports System.Collections.ObjectModel
Imports System.Collections.Specialized
Imports System.Reflection
Imports System.Text
Imports Bsharp.IO

Namespace Bsharp

  Friend MustInherit Class Repl

    Private ReadOnly m_metaCommands As New List(Of MetaCommand)
    Friend m_immediateCommands As List(Of String)
    Private ReadOnly m_submissionHistory As New List(Of String)
    Private m_submissionHistoryIndex As Integer
    Private m_done As Boolean = False

    Friend m_edit As String = ""

    Friend Sub New()
      InitializeMetaCommands()
    End Sub

    Private Sub InitializeMetaCommands()

      Dim methods = [GetType]().GetMethods(BindingFlags.Public Or
                                           BindingFlags.NonPublic Or
                                           BindingFlags.Static Or
                                           BindingFlags.Instance Or
                                           BindingFlags.FlattenHierarchy)

      For Each method In methods

        Dim attribute = method.GetCustomAttribute(Of MetaCommandAttribute)
        If attribute Is Nothing Then Continue For

        Dim metaCommand = New MetaCommand(attribute.Name, attribute.Description, method)
        m_metaCommands.Add(metaCommand)

      Next

    End Sub

    Friend m_fullScreenEditor As Boolean = False

    Public Sub Run()

      Do

        If m_fullScreenEditor Then

          Console.Clear()

          Console.SetCursorPosition(0, Console.WindowHeight - 3)
          Console.ForegroundColor = ConsoleColor.Magenta
          Dim line = New String("─"c, Console.WindowWidth - 1)
          Console.WriteLine(line)
          Dim statusTop = Console.WindowHeight - 2
          Console.SetCursorPosition(0, statusTop)
          Console.Write("ESC:Exit  F1:Help  F2:      F3:      F4:      F5:Run")
          Dim statusLeft = Console.WindowWidth - 20
          Console.SetCursorPosition(statusLeft, statusTop)
          Console.Write("Ln:    Col:     INS")
          Console.ResetColor()

          Dim maxLeft = Console.WindowWidth - 2
          Dim maxTop = Console.WindowHeight - 4

          Dim document = New ObservableCollection(Of String) 'From {""}
          For Each line In Split(m_edit, vbCrLf)
            document.Add(line)
          Next
          Dim view = New SubmissionView2(AddressOf RenderLine, document)

          Console.SetCursorPosition(0, 0)

          Dim run = False

          Do

            Dim key = Console.ReadKey(True)

            Dim handled = False
            If key.Modifiers = 0 Then
              Select Case key.Key
                Case ConsoleKey.Escape : handled = True : Exit Do
                Case ConsoleKey.F5 : run = True : handled = True : Exit Do
                Case Else
              End Select
            End If
            If Not handled Then
              view.HandleKey(key, document)
            End If

            Dim currentLeft = Console.CursorLeft
            Dim currentTop = Console.CursorTop

            Console.CursorVisible = False
            Try
              Console.ForegroundColor = ConsoleColor.Magenta
              Console.SetCursorPosition(statusLeft + 4, statusTop)
              Console.Write(currentTop + 1)
              Console.SetCursorPosition(statusLeft + 12, statusTop)
              Console.Write(currentLeft + 1)
              Console.SetCursorPosition(statusLeft + 16, statusTop)
              Console.Write(If(True, "INS", "OVR"))
              Console.ResetColor()
            Finally
              Console.CursorVisible = True
            End Try

            Console.SetCursorPosition(currentLeft, currentTop)

          Loop

          m_edit = String.Join(Environment.NewLine, document)
          m_fullScreenEditor = False
          Console.Clear()

          If run Then
            EvaluateSubmission(m_edit)
          End If

        Else

          Dim text = EditSubmission()
          If String.IsNullOrEmpty(text) Then
            Continue Do
          End If

          Dim immediateCommand = False
          If Not text.Contains(Environment.NewLine) Then
            Dim word = text?.Split(" "c)?.First
            If word.StartsWith("$") Then immediateCommand = True
            If m_immediateCommands.Contains(word.ToLower) Then immediateCommand = True
          End If

          If immediateCommand Then
            EvaluateMetaCommand(text)
          Else
            EvaluateSubmission(text)
          End If

          m_submissionHistory.Add(text)
          m_submissionHistoryIndex = 0

        End If

      Loop

    End Sub

    Private Delegate Function LineRenderHandler(lines As IReadOnlyList(Of String), lineIndex As Integer, state As Object) As Object

    Private NotInheritable Class SubmissionView2

      Private ReadOnly m_lineRenderer As LineRenderHandler
      Private WithEvents SubmissionDocument As ObservableCollection(Of String)
      'Private m_cursorTop As Integer
      Private m_renderedLineCount As Integer
      Private m_currentLine As Integer
      Private m_currentCharacter As Integer

      'Private ReadOnly m_maxLeft As Integer
      Private ReadOnly m_maxTop As Integer

      Sub New(lineRenderer As LineRenderHandler, submissionDocument As ObservableCollection(Of String))
        m_lineRenderer = lineRenderer
        Me.SubmissionDocument = submissionDocument
        'm_cursorTop = Console.CursorTop
        'm_maxLeft = Console.WindowWidth - 2
        m_maxTop = Console.WindowHeight - 4
        Render()
      End Sub

      Private Sub CollectionChanged(sender As Object, e As NotifyCollectionChangedEventArgs) Handles SubmissionDocument.CollectionChanged
        Render()
      End Sub

      Private Sub Render()

        'Console.SetCursorPosition(0, Me.m_cursorTop)
        Console.CursorVisible = False

        Dim lineCount = 0
        Dim state = CObj(Nothing)

        For Each line In SubmissionDocument

          If lineCount > m_maxTop Then
            Console.SetCursorPosition(0, Console.WindowHeight - 1)
            Console.WriteLine()
          End If

          Console.SetCursorPosition(0, lineCount)
          state = m_lineRenderer(SubmissionDocument, lineCount, state)
          Console.Write(New String(" "c, Console.WindowWidth - line.Length - 2))
          lineCount += 1

        Next

        Dim numberOfBlankLines = m_renderedLineCount - lineCount
        If numberOfBlankLines > 0 Then
          Dim blankLine = New String(" "c, Console.WindowWidth)
          For i = 0 To numberOfBlankLines - 1
            Console.SetCursorPosition(0, lineCount + i)
            Console.WriteLine(blankLine)
          Next
        End If

        m_renderedLineCount = lineCount

        Console.CursorVisible = True
        UpdateCursorPosition()

      End Sub

      Private Sub UpdateCursorPosition()
        Console.CursorTop = m_currentLine
        Console.CursorLeft = m_currentCharacter
      End Sub

      Public Property CurrentLine As Integer
        Get
          Return m_currentLine
        End Get
        Set
          If m_currentLine <> Value Then
            m_currentLine = Value
            m_currentCharacter = Math.Min(SubmissionDocument(m_currentLine).Length, m_currentCharacter)
            UpdateCursorPosition()
          End If
        End Set
      End Property

      Public Property CurrentCharacter As Integer
        Get
          Return m_currentCharacter
        End Get
        Set
          If m_currentCharacter <> Value Then
            m_currentCharacter = Value
            UpdateCursorPosition()
          End If
        End Set
      End Property

      Public Sub HandleKey(key As ConsoleKeyInfo, document As ObservableCollection(Of String))
        If key.Modifiers = 0 Then
          Select Case key.Key
            'Case ConsoleKey.Escape : HandleEscape(document)
            Case ConsoleKey.Enter : HandleEnter(document)
            Case ConsoleKey.LeftArrow : HandleLeftArrow(document)
            Case ConsoleKey.RightArrow : HandleRightArrow(document)
            Case ConsoleKey.UpArrow : HandleUpArrow(document)
            Case ConsoleKey.DownArrow : HandleDownArrow(document)
            Case ConsoleKey.Backspace : HandleBackspace(document)
            Case ConsoleKey.Delete : HandleDelete(document)
            Case ConsoleKey.Home : HandleHome(document)
            Case ConsoleKey.End : HandleEnd(document)
            Case ConsoleKey.Tab : HandleTab(document)
            Case ConsoleKey.PageUp : HandlePageUp(document)
            Case ConsoleKey.PageDown : HandlePageDown(document)
            Case Else
          End Select
        ElseIf key.Modifiers = ConsoleModifiers.Control Then
          Select Case key.Key
            Case ConsoleKey.Enter
              HandleControlEnter(document)
            Case Else
          End Select
        End If
        If key.Key <> ConsoleKey.Backspace AndAlso key.KeyChar >= " "c Then
          HandleTyping(document, key.KeyChar.ToString)
        End If
      End Sub

      'Private Sub HandleEscape(document As ObservableCollection(Of String))
      '  document.Clear()
      '  document.Add(String.Empty)
      '  Me.CurrentLine = 0
      '  Me.CurrentCharacter = 0
      'End Sub

      Private Sub HandleEnter(document As ObservableCollection(Of String))
        InsertLine(document)
      End Sub

      Private Sub HandleControlEnter(document As ObservableCollection(Of String))
        InsertLine(document)
      End Sub

      Private Sub HandleLeftArrow(document As ObservableCollection(Of String))
        If document Is Nothing Then
        End If
        If Me.CurrentCharacter > 0 Then
          Me.CurrentCharacter -= 1
        End If
      End Sub

      Private Sub HandleRightArrow(document As ObservableCollection(Of String))
        Dim line = document(Me.CurrentLine)
        If Me.CurrentCharacter <= line.Length - 1 Then
          Me.CurrentCharacter += 1
        End If
      End Sub

      Private Sub HandleUpArrow(document As ObservableCollection(Of String))
        If document Is Nothing Then
        End If
        If Me.CurrentLine > 0 Then
          Me.CurrentLine -= 1
        End If
      End Sub

      Private Sub HandleDownArrow(document As ObservableCollection(Of String))
        If Me.CurrentLine < document.Count - 1 Then
          Me.CurrentLine += 1
        End If
      End Sub

      Private Sub HandleBackspace(document As ObservableCollection(Of String))
        Dim start = Me.CurrentCharacter
        If start = 0 Then
          If Me.CurrentLine = 0 Then Return
          Dim currentLine = document(Me.CurrentLine)
          Dim previousLine = document(Me.CurrentLine - 1)
          document.RemoveAt(Me.CurrentLine)
          Me.CurrentLine -= 1
          document(Me.CurrentLine) = previousLine & currentLine
          Me.CurrentCharacter = previousLine.Length
        Else
          Dim lineIndex = Me.CurrentLine
          Dim line = document(lineIndex)
          Dim before = line.Substring(0, start - 1)
          Dim after = line.Substring(start)
          document(lineIndex) = before & after
          Me.CurrentCharacter -= 1
        End If
      End Sub

      Private Sub HandleDelete(document As ObservableCollection(Of String))
        Dim lineIndex = Me.CurrentLine
        Dim line = document(lineIndex)
        Dim start = Me.CurrentCharacter
        If start >= line.Length Then
          If Me.CurrentLine = document.Count - 1 Then Return
          Dim nextLine = document(Me.CurrentLine + 1)
          document(Me.CurrentLine) &= nextLine
          document.RemoveAt(Me.CurrentLine + 1)
          Return
        End If
        Dim before = line.Substring(0, start)
        Dim after = line.Substring(start + 1)
        document(lineIndex) = before & after
      End Sub

      Private Sub HandleHome(document As ObservableCollection(Of String))
        If document Is Nothing Then
        End If
        Me.CurrentCharacter = 0
      End Sub

      Private Sub HandleEnd(document As ObservableCollection(Of String))
        Me.CurrentCharacter = document(Me.CurrentLine).Length
      End Sub

      Private Shared Sub HandlePageUp(document As ObservableCollection(Of String))
        If document Is Nothing Then
        End If
      End Sub

      Private Shared Sub HandlePageDown(document As ObservableCollection(Of String))
        If document Is Nothing Then
        End If
      End Sub

      Private Sub InsertLine(document As ObservableCollection(Of String))
        Dim remainder = document(Me.CurrentLine).Substring(Me.CurrentCharacter)
        document(Me.CurrentLine) = document(Me.CurrentLine).Substring(0, Me.CurrentCharacter)
        Dim lineIndex = Me.CurrentLine + 1
        document.Insert(lineIndex, remainder)
        Me.CurrentCharacter = 0
        Me.CurrentLine = lineIndex
      End Sub

      Private Sub HandleTab(document As ObservableCollection(Of String))
        Const TAB_WIDTH As Integer = 2
        Dim start = Me.CurrentCharacter
        Dim remainingSpaces = TAB_WIDTH - start Mod TAB_WIDTH
        Dim line = document(Me.CurrentLine)
        document(Me.CurrentLine) = line.Insert(start, New String(" "c, remainingSpaces))
        Me.CurrentCharacter += remainingSpaces
      End Sub

      Private Sub HandleTyping(document As ObservableCollection(Of String), text As String)
        Dim lineIndex = Me.CurrentLine
        Dim start = Me.CurrentCharacter
        document(lineIndex) = document(lineIndex).Insert(start, text)
        Me.CurrentCharacter += text.Length
      End Sub

    End Class

    Private NotInheritable Class SubmissionView

      Private ReadOnly m_lineRenderer As LineRenderHandler
      Private WithEvents SubmissionDocument As ObservableCollection(Of String)
      Private m_cursorTop As Integer
      Private m_renderedLineCount As Integer
      Private m_currentLine As Integer
      Private m_currentCharacter As Integer

      Sub New(lineRenderer As LineRenderHandler, submissionDocument As ObservableCollection(Of String))
        m_lineRenderer = lineRenderer
        Me.SubmissionDocument = submissionDocument
        m_cursorTop = Console.CursorTop
        Render()
      End Sub

      Public Property CursorTop As Integer
        Get
          Return m_cursorTop
        End Get
        Set(value As Integer)
          m_cursorTop = value
          Render()
        End Set
      End Property

      Private Sub CollectionChanged(sender As Object, e As NotifyCollectionChangedEventArgs) Handles SubmissionDocument.CollectionChanged
        Render()
      End Sub

      Private Sub Render()

        'Console.SetCursorPosition(0, Me.m_cursorTop)
        Console.CursorVisible = False

        Dim lineCount = 0
        Dim state = CObj(Nothing)

        For Each line In SubmissionDocument

          If m_cursorTop + lineCount > Console.WindowHeight Then '>= Console.WindowHeight Then
            Console.SetCursorPosition(0, Console.WindowHeight - 1)
            Console.WriteLine()
            'If m_cursorTop > 0 Then m_cursorTop -= 1
          End If

          Console.SetCursorPosition(0, m_cursorTop + lineCount)
          Console.ForegroundColor = ConsoleColor.Green

          If lineCount = 0 Then
            'Console.Write("» ")
            Console.Write("> ")
          Else
            Console.Write("· ")
          End If

          Console.ResetColor()
          state = m_lineRenderer(SubmissionDocument, lineCount, state)
          Console.Write(New String(" "c, Console.WindowWidth - line.Length - 2))
          lineCount += 1

        Next

        Dim numberOfBlankLines = m_renderedLineCount - lineCount
        If numberOfBlankLines > 0 Then
          Dim blankLine = New String(" "c, Console.WindowWidth)
          For i = 0 To numberOfBlankLines - 1
            Console.SetCursorPosition(0, m_cursorTop + lineCount + i)
            Console.WriteLine(blankLine)
          Next
        End If

        m_renderedLineCount = lineCount

        Console.CursorVisible = True
        UpdateCursorPosition()

      End Sub

      Private Sub UpdateCursorPosition()
        'Console.SetCursorPosition(2 + CurrentCharacter, m_cursorTop + CurrentLine)
        Console.CursorTop = m_cursorTop + m_currentLine
        Console.CursorLeft = 2 + m_currentCharacter
      End Sub

      Public Property CurrentLine As Integer
        Get
          Return m_currentLine
        End Get
        Set
          If m_currentLine <> Value Then
            m_currentLine = Value
            m_currentCharacter = Math.Min(SubmissionDocument(m_currentLine).Length, m_currentCharacter)
            UpdateCursorPosition()
          End If
        End Set
      End Property

      Public Property CurrentCharacter As Integer
        Get
          Return m_currentCharacter
        End Get
        Set
          If m_currentCharacter <> Value Then
            m_currentCharacter = Value
            UpdateCursorPosition()
          End If
        End Set
      End Property

    End Class

    Friend Sub LoadDocument(text As String)
      Dim document = New ObservableCollection(Of String) From {""}
      Dim view = New SubmissionView(AddressOf RenderLine, document)
      document.Clear()
      Dim historyItem = text
      Dim lines = historyItem.Split(Environment.NewLine)
      For Each line In lines
        document.Add(line)
      Next
      view.CurrentLine = document.Count - 1
      view.CurrentCharacter = document(view.CurrentLine).Length
    End Sub

    Private Function EditSubmission() As String

      m_done = False

      Dim document = New ObservableCollection(Of String) From {""}
      Dim view = New SubmissionView(AddressOf RenderLine, document)

      While Not m_done
        Dim key As ConsoleKeyInfo
        Try
          key = Console.ReadKey(True)
        Catch ex As InvalidOperationException
          ' Because we closed the console window (X)
          Exit While
        End Try
        HandleKey(key, document, view)
      End While

      view.CurrentLine = document.Count - 1
      view.CurrentCharacter = document(view.CurrentLine).Length
      Console.WriteLine()

      Return String.Join(Environment.NewLine, document)

    End Function

    Private Sub HandleKey(key As ConsoleKeyInfo, document As ObservableCollection(Of String), view As SubmissionView)
      If key.Modifiers = 0 Then
        Select Case key.Key
          Case ConsoleKey.Escape : HandleEscape(document, view)
          Case ConsoleKey.Enter : HandleEnter(document, view)
          Case ConsoleKey.LeftArrow : HandleLeftArrow(document, view)
          Case ConsoleKey.RightArrow : HandleRightArrow(document, view)
          Case ConsoleKey.UpArrow : HandleUpArrow(document, view)
          Case ConsoleKey.DownArrow : HandleDownArrow(document, view)
          Case ConsoleKey.Backspace : HandleBackspace(document, view)
          Case ConsoleKey.Delete : HandleDelete(document, view)
          Case ConsoleKey.Home : HandleHome(document, view)
          Case ConsoleKey.End : HandleEnd(document, view)
          Case ConsoleKey.Tab : HandleTab(document, view)
          Case ConsoleKey.PageUp : HandlePageUp(document, view)
          Case ConsoleKey.PageDown : HandlePageDown(document, view)
          Case Else
        End Select
      ElseIf key.Modifiers = ConsoleModifiers.Control Then
        Select Case key.Key
          Case ConsoleKey.Enter
            HandleControlEnter(document, view)
          Case Else
        End Select
      End If
      If key.Key <> ConsoleKey.Backspace AndAlso key.KeyChar >= " "c Then
        HandleTyping(document, view, key.KeyChar.ToString)
      End If
    End Sub

    Private Shared Sub HandleEscape(document As ObservableCollection(Of String), view As SubmissionView)
      document.Clear()
      document.Add(String.Empty)
      view.CurrentLine = 0
      view.CurrentCharacter = 0
    End Sub

    Private Sub HandleEnter(document As ObservableCollection(Of String), view As SubmissionView)
      Dim submissionText = String.Join(Environment.NewLine, document)
      If submissionText.StartsWith("$") OrElse IsCompleteSubmission(submissionText) Then
        m_done = True
        Return
      End If
      InsertLine(document, view)
    End Sub

    Private Shared Sub HandleControlEnter(document As ObservableCollection(Of String), view As SubmissionView)
      InsertLine(document, view)
    End Sub

    Private Shared Sub InsertLine(document As ObservableCollection(Of String), view As SubmissionView)
      Dim remainder = document(view.CurrentLine).Substring(view.CurrentCharacter)
      document(view.CurrentLine) = document(view.CurrentLine).Substring(0, view.CurrentCharacter)
      Dim lineIndex = view.CurrentLine + 1
      document.Insert(lineIndex, remainder)
      view.CurrentCharacter = 0
      view.CurrentLine = lineIndex
    End Sub

    Private Shared Sub HandleLeftArrow(document As ObservableCollection(Of String), view As SubmissionView)
      If document Is Nothing Then
      End If
      If view.CurrentCharacter > 0 Then
        view.CurrentCharacter -= 1
      End If
    End Sub

    Private Shared Sub HandleRightArrow(document As ObservableCollection(Of String), view As SubmissionView)
      Dim line = document(view.CurrentLine)
      If view.CurrentCharacter <= line.Length - 1 Then
        view.CurrentCharacter += 1
      End If
    End Sub

    Private Shared Sub HandleUpArrow(document As ObservableCollection(Of String), view As SubmissionView)
      If document Is Nothing Then
      End If
      If view.CurrentLine > 0 Then
        view.CurrentLine -= 1
      End If
    End Sub

    Private Shared Sub HandleDownArrow(document As ObservableCollection(Of String), view As SubmissionView)
      If view.CurrentLine < document.Count - 1 Then
        view.CurrentLine += 1
      End If
    End Sub

    Private Shared Sub HandleBackspace(document As ObservableCollection(Of String), view As SubmissionView)
      Dim start = view.CurrentCharacter
      If start = 0 Then
        If view.CurrentLine = 0 Then Return
        Dim currentLine = document(view.CurrentLine)
        Dim previousLine = document(view.CurrentLine - 1)
        document.RemoveAt(view.CurrentLine)
        view.CurrentLine -= 1
        document(view.CurrentLine) = previousLine & currentLine
        view.CurrentCharacter = previousLine.Length
      Else
        Dim lineIndex = view.CurrentLine
        Dim line = document(lineIndex)
        Dim before = line.Substring(0, start - 1)
        Dim after = line.Substring(start)
        document(lineIndex) = before & after
        view.CurrentCharacter -= 1
      End If
    End Sub

    Private Shared Sub HandleDelete(document As ObservableCollection(Of String), view As SubmissionView)
      Dim lineIndex = view.CurrentLine
      Dim line = document(lineIndex)
      Dim start = view.CurrentCharacter
      If start >= line.Length Then
        If view.CurrentLine = document.Count - 1 Then Return
        Dim nextLine = document(view.CurrentLine + 1)
        document(view.CurrentLine) &= nextLine
        document.RemoveAt(view.CurrentLine + 1)
        Return
      End If
      Dim before = line.Substring(0, start)
      Dim after = line.Substring(start + 1)
      document(lineIndex) = before & after
    End Sub

    Private Shared Sub HandleHome(document As ObservableCollection(Of String), view As SubmissionView)
      If document Is Nothing Then
      End If
      view.CurrentCharacter = 0
    End Sub

    Private Shared Sub HandleEnd(document As ObservableCollection(Of String), view As SubmissionView)
      view.CurrentCharacter = document(view.CurrentLine).Length
    End Sub

    Private Sub HandlePageUp(document As ObservableCollection(Of String), view As SubmissionView)
      m_submissionHistoryIndex -= 1
      If m_submissionHistoryIndex < 0 Then
        m_submissionHistoryIndex = m_submissionHistory.Count - 1
      End If
      UpdateDocumentFromHistory(document, view)
    End Sub

    Private Sub HandlePageDown(document As ObservableCollection(Of String), view As SubmissionView)
      m_submissionHistoryIndex += 1
      If m_submissionHistoryIndex > m_submissionHistory.Count - 1 Then
        m_submissionHistoryIndex = 0
      End If
      UpdateDocumentFromHistory(document, view)
    End Sub

    Private Sub UpdateDocumentFromHistory(document As ObservableCollection(Of String), view As SubmissionView)
      If m_submissionHistory.Count = 0 Then Return
      document.Clear()
      Dim historyItem = m_submissionHistory(m_submissionHistoryIndex)
      Dim lines = historyItem.Split(Environment.NewLine)
      For Each line In lines
        document.Add(line)
      Next
      view.CurrentLine = document.Count - 1
      view.CurrentCharacter = document(view.CurrentLine).Length
    End Sub

    Private Shared Sub HandleTab(document As ObservableCollection(Of String), view As SubmissionView)
      Const TAB_WIDTH As Integer = 2
      Dim start = view.CurrentCharacter
      Dim remainingSpaces = TAB_WIDTH - start Mod TAB_WIDTH
      Dim line = document(view.CurrentLine)
      document(view.CurrentLine) = line.Insert(start, New String(" "c, remainingSpaces))
      view.CurrentCharacter += remainingSpaces
    End Sub

    Private Shared Sub HandleTyping(document As ObservableCollection(Of String), view As SubmissionView, text As String)
      Dim lineIndex = view.CurrentLine
      Dim start = view.CurrentCharacter
      document(lineIndex) = document(lineIndex).Insert(start, text)
      view.CurrentCharacter += text.Length
    End Sub

    Protected Sub EvaluateMetaCommand(input As String)

      ' Parse arguments

      Dim args = New List(Of String)
      Dim inQuotes = False
      Dim position = If(input.StartsWith("$"), 1, 0)
      Dim sb = New StringBuilder
      While position < input.Length

        Dim c = input(position)
        Dim l = If(position + 1 >= input.Length, Chr(0), input(position + 1))

        If Char.IsWhiteSpace(c) Then
          If Not inQuotes Then
            CommitPendingArgument(args, sb)
          Else
            sb.Append(c)
          End If
        ElseIf c = Chr(34) Then
          If Not inQuotes Then
            inQuotes = True
          ElseIf l = Chr(34) Then
            sb.Append(c)
            position += 1
          Else
            inQuotes = False
          End If
        Else
          sb.Append(c)
        End If

        position += 1

      End While

      CommitPendingArgument(args, sb)

      Dim commandName = args.FirstOrDefault

      If args.Count > 0 Then
        args.RemoveAt(0)
      End If

      Dim command = m_metaCommands.SingleOrDefault(Function(mc) mc.Name = commandName)

      If command Is Nothing Then
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine($"Invalid command {input}.")
        Console.ResetColor()
        Return
      End If

      Dim parameters = command.Method.GetParameters

      If args.Count <> parameters.Length Then
        Dim parameterNames = String.Join(" ", parameters.Select(Function(p) $"<{p.Name}>"))
        Console.ForegroundColor = ConsoleColor.Red
        Console.WriteLine($"error: invalid number of arguments")
        Console.WriteLine($"usage: #{command.Name} {parameterNames}")
        Console.ResetColor()
        Return
      End If

      'command.Method.Invoke(Me, args.ToArray)
      Dim instance = If(command.Method.IsStatic, Nothing, Me)
      command.Method.Invoke(instance, args.ToArray)

    End Sub

    Private Shared Sub CommitPendingArgument(args As List(Of String), sb As StringBuilder)

      Dim arg = sb.ToString

      If Not String.IsNullOrWhiteSpace(arg) Then
        args.Add(arg)
      End If

      sb.Clear()

    End Sub

    Protected Sub ClearHistory()
      m_submissionHistory.Clear()
    End Sub

    Protected Overridable Function RenderLine(lines As IReadOnlyList(Of String), lineIndex As Integer, state As Object) As Object
      Console.Write(lines(lineIndex))
      Return state
    End Function

    Protected MustOverride Function IsCompleteSubmission(text As String) As Boolean

    Protected MustOverride Sub EvaluateSubmission(text As String)

    <AttributeUsage(AttributeTargets.Method, AllowMultiple:=False)>
    Protected NotInheritable Class MetaCommandAttribute
      Inherits Attribute

      Sub New(name As String, description As String)
        Me.Name = name
        Me.Description = description
      End Sub

      Public ReadOnly Property Name As String
      Public ReadOnly Property Description As String

    End Class

    Private Class MetaCommand

      Public Sub New(name As String, description As String, method As MethodInfo)
        Me.Name = name
        Me.Description = description
        Me.Method = method
      End Sub

      Public ReadOnly Property Name As String
      Public ReadOnly Property Description As String
      Public ReadOnly Property Method As MethodInfo

    End Class

    <MetaCommand("help", "Shows help")>
    Protected Sub EvaluateHelp()

      Dim maxNameLength = m_metaCommands.Max(Function(mc) mc.Name.Length)

      For Each metaCommand In m_metaCommands.OrderBy(Function(mc) mc.Name)
        'Dim paddedName = metaCommand.Name.PadRight(maxNameLength)

        Dim metaParams = metaCommand.Method.GetParameters
        If metaParams.Length = 0 Then
          Dim paddedName = metaCommand.Name.PadRight(maxNameLength)
          Console.Out.WritePunctuation("$")
          Console.Out.WriteIdentifier(paddedName)
        Else
          Console.Out.WritePunctuation("$")
          Console.Out.WriteIdentifier(metaCommand.Name)
          For Each pi In metaParams
            Console.Out.WriteSpace()
            Console.Out.WritePunctuation("<")
            Console.Out.WriteIdentifier(pi.Name)
            Console.Out.WritePunctuation(">")
          Next
          Console.Out.WriteLine()
          Console.Out.WriteSpace
          Console.Out.Write(Space(maxNameLength))
        End If

        'Console.Out.WritePunctuation("#")
        'Console.Out.WriteIdentifier(paddedName)
        Console.Out.WriteSpace()
        Console.Out.WriteSpace()
        Console.Out.WriteSpace()
        Console.Out.WritePunctuation(metaCommand.Description)
        Console.Out.WriteLine()

      Next

    End Sub

  End Class

End Namespace
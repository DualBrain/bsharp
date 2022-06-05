Imports System.Collections.ObjectModel
Imports System.Collections.Specialized

Namespace Basic

  Friend MustInherit Class Repl

    Private ReadOnly m_submissionHistory As New List(Of String)
    Private m_submissionHistoryIndex As Integer
    Private m_done As Boolean
    Friend m_exit As Boolean

    Public Sub Run()
      Do
        Dim text = EditSubmission()
        If String.IsNullOrEmpty(text) Then Return
        If Not text.Contains(Environment.NewLine) AndAlso text.StartsWith("$") Then
          EvaluateMetaCommand(text)
          If m_exit Then Exit Do
        Else
          EvaluateSubmission(text)
        End If
        m_submissionHistory.Add(text)
        m_submissionHistoryIndex = 0
      Loop
    End Sub

    Private NotInheritable Class SubmissionView

      Private ReadOnly m_lineRenderer As Action(Of String)
      Private ReadOnly m_submissionDocument As ObservableCollection(Of String)
      Private ReadOnly m_cursorTop As Integer

      Private m_renderedLineCount As Integer
      Private m_currentLine As Integer
      Private m_currentCharacter As Integer

      Public Sub New(lineRenderer As Action(Of String), submissionDocument As ObservableCollection(Of String))
        m_lineRenderer = lineRenderer
        m_submissionDocument = submissionDocument
        AddHandler m_submissionDocument.CollectionChanged, AddressOf SubmissionDocumentChanged
        m_cursorTop = Console.CursorTop
        Render()
      End Sub

      Private Sub SubmissionDocumentChanged(sender As Object, e As NotifyCollectionChangedEventArgs)
        Render()
      End Sub

      Private Sub Render()

        Console.CursorVisible = False

        Dim lineCount = 0

        For Each line In m_submissionDocument

          Console.SetCursorPosition(0, m_cursorTop + lineCount)
          Console.ForegroundColor = ConsoleColor.Green

          If lineCount = 0 Then
            Console.Write("» ")
          Else Console.Write("· ")
          End If

          Console.ResetColor()
          m_lineRenderer(line)
          Console.WriteLine(New String(" "c, Console.WindowWidth - line.Length))

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
        Console.CursorTop = m_cursorTop + m_currentLine
        Console.CursorLeft = 2 + m_currentCharacter
      End Sub

      Public Property CurrentLine As Integer
        Get
          Return m_currentLine
        End Get
        Set(value As Integer)
          If m_currentLine <> value Then
            m_currentLine = value
            m_currentCharacter = Math.Min(m_submissionDocument(m_currentLine).Length, m_currentCharacter)
            UpdateCursorPosition()
          End If
        End Set
      End Property

      Public Property CurrentCharacter As Integer
        Get
          Return m_currentCharacter
        End Get
        Set(value As Integer)
          If m_currentCharacter <> value Then
            m_currentCharacter = value
            UpdateCursorPosition()
          End If
        End Set
      End Property

    End Class

    Private Function EditSubmission() As String

      m_done = False

      Dim document = New ObservableCollection(Of String) From {""}
      Dim view = New SubmissionView(AddressOf RenderLine, document)

      While Not m_done
        Dim key = Console.ReadKey(True)
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
          Case ConsoleKey.Enter : HandleControlEnter(document, view)
          Case Else
        End Select
      End If
      If key.KeyChar >= " "c Then
        HandleTyping(document, view, key.KeyChar.ToString())
      End If
    End Sub

    Private Shared Sub HandleEscape(document As ObservableCollection(Of String), view As SubmissionView)
      'document(view.CurrentLine) = String.Empty
      'view.CurrentCharacter = 0
      document.Clear()
      document.Add(String.Empty)
      view.CurrentLine = 0
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
      If document IsNot Nothing Then
      End If
      If view.CurrentCharacter > 0 Then view.CurrentCharacter -= 1
    End Sub

    Private Shared Sub HandleRightArrow(document As ObservableCollection(Of String), view As SubmissionView)
      Dim line = document(view.CurrentLine)
      If view.CurrentCharacter <= line.Length - 1 Then view.CurrentCharacter += 1
    End Sub

    Private Shared Sub HandleUpArrow(document As ObservableCollection(Of String), view As SubmissionView)
      If document IsNot Nothing Then
      End If
      If view.CurrentLine > 0 Then view.CurrentLine -= 1
    End Sub

    Private Shared Sub HandleDownArrow(document As ObservableCollection(Of String), view As SubmissionView)
      If view.CurrentLine < document.Count - 1 Then view.CurrentLine += 1
    End Sub

    Private Shared Sub HandleBackspace(document As ObservableCollection(Of String), view As SubmissionView)
      Dim start = view.CurrentCharacter
      If start = 0 Then
        If view.CurrentLine = 0 Then Return
        Dim currentLine1 = document(view.CurrentLine)
        Dim previousLine = document(view.CurrentLine - 1)
        document.RemoveAt(view.CurrentLine)
        view.CurrentLine -= 1
        document(view.CurrentLine) = previousLine & currentLine1
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
      If document IsNot Nothing Then
      End If
      view.CurrentCharacter = 0
    End Sub

    Private Shared Sub HandleEnd(document As ObservableCollection(Of String), view As SubmissionView)
      view.CurrentCharacter = document(view.CurrentLine).Length
    End Sub

    Private Shared Sub HandleTab(document As ObservableCollection(Of String), view As SubmissionView)
      Dim tabWidth = 4
      Dim start = view.CurrentCharacter
      Dim remainingSpaces = TabWidth - start Mod TabWidth
      Dim line = document(view.CurrentLine)
      document(view.CurrentLine) = line.Insert(start, New String(" "c, remainingSpaces))
      view.CurrentCharacter += remainingSpaces
    End Sub

    Private Sub HandlePageUp(document As ObservableCollection(Of String), view As SubmissionView)
      m_submissionHistoryIndex -= 1
      If m_submissionHistoryIndex < 0 Then m_submissionHistoryIndex = m_submissionHistory.Count - 1
      UpdateDocumentFromHistory(document, view)
    End Sub

    Private Sub HandlePageDown(document As ObservableCollection(Of String), view As SubmissionView)
      m_submissionHistoryIndex += 1
      If m_submissionHistoryIndex > m_submissionHistory.Count - 1 Then m_submissionHistoryIndex = 0
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

    Private Shared Sub HandleTyping(document As ObservableCollection(Of String), view As SubmissionView, text As String)
      Dim lineIndex = view.CurrentLine
      Dim start = view.CurrentCharacter
      document(lineIndex) = document(lineIndex).Insert(start, text)
      view.CurrentCharacter += text.Length
    End Sub

    Protected Sub ClearHistory()
      m_submissionHistory.Clear()
    End Sub

    Protected Overridable Sub RenderLine(line As String)
      Console.Write(line)
    End Sub

    Protected Overridable Sub EvaluateMetaCommand(input As String)
      Console.ForegroundColor = ConsoleColor.Red
      Console.WriteLine($"Invalid command {input}.")
      Console.ResetColor()
    End Sub

    Protected MustOverride Function IsCompleteSubmission(text As String) As Boolean

    Protected MustOverride Sub EvaluateSubmission(text As String)

  End Class

End Namespace
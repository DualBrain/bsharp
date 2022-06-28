Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Text

  Public NotInheritable Class SourceText

    Private ReadOnly Text As String

    Public ReadOnly Property FileName As String

    Private Sub New(text As String, fileName As String)
      Me.Text = text
      Me.FileName = fileName
      Lines = ParseLines(Me, text)
    End Sub

    ''' <summary>
    ''' Parses source text into lines/spans.
    ''' </summary>
    ''' <param name="text">source text</param>
    ''' <param name="fileName">the filename (for reference)</param>
    ''' <returns></returns>
    Public Shared Function [From](text As String, Optional fileName As String = "") As SourceText
      Return New SourceText(text, fileName)
    End Function

    ''' <summary>
    ''' Contains zero or more parsed source lines.
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property Lines As ImmutableArray(Of TextLine)

    ''' <summary>
    ''' Provides access to each character of the source text.
    ''' </summary>
    ''' <param name="index">character index</param>
    ''' <returns>character at index</returns>
    Default Public ReadOnly Property Item(index As Integer) As Char
      Get
        Return Text(index)
      End Get
    End Property

    ''' <summary>
    ''' Overall length of source text.
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property Length As Integer
      Get
        Return Text.Length
      End Get
    End Property

    ''' <summary>
    ''' Returns line index given character position.
    ''' </summary>
    ''' <param name="position">character index</param>
    ''' <returns>line index</returns>
    Public Function GetLineIndex(position As Integer) As Integer

      ' Implementing a "Binary Search".

      Dim lower = 0
      Dim upper = Lines.Length - 1

      While lower <= upper

        Dim index = lower + ((upper - lower) \ 2)
        Dim start = Lines(index).Start

        If position = start Then
          ' Found it!
          Return index
        End If

        If start > position Then
          ' "discard" the upper window.
          upper = index - 1
        Else
          ' "discard" the lower window.
          lower = index + 1
        End If

      End While

      ' We've run out of stuff to search, return where we ended up.
      Return lower - 1

    End Function

    Private Shared Function ParseLines(sourceText As SourceText, text As String) As ImmutableArray(Of TextLine)

      Dim result = ImmutableArray.CreateBuilder(Of TextLine)

      Dim position = 0
      Dim lineStart = 0

      While position < text.Length
        Dim lineBreakWidth = GetLineBreakWidth(text, position)
        If lineBreakWidth = 0 Then
          position += 1
        Else
          AddLine(result, sourceText, position, lineStart, lineBreakWidth)
          position += lineBreakWidth
          lineStart = position
        End If
      End While

      If position >= lineStart Then
        AddLine(result, sourceText, position, lineStart, 0)
      End If

      Return result.ToImmutable

    End Function

    Private Shared Sub AddLine(result As ImmutableArray(Of TextLine).Builder, sourceText As SourceText, position As Integer, lineStart As Integer, lineBreakWidth As Integer)
      Dim lineLength = position - lineStart
      Dim lineLengthIncludingLineBreak = lineLength + lineBreakWidth
      Dim line = New TextLine(sourceText, lineStart, lineLength, lineLengthIncludingLineBreak)
      result.Add(line)
    End Sub

    Private Shared Function GetLineBreakWidth(text As String, position As Integer) As Integer

      Dim c = text(position)
      Dim l = If(position >= text.Length - 1, ChrW(0), text(position + 1))

      If c = ChrW(13) AndAlso l = ChrW(10) Then
        Return 2
      ElseIf c = ChrW(13) OrElse c = ChrW(10) Then
        Return 1
      Else
        Return 0
      End If

    End Function

    ''' <summary>
    ''' Source Text
    ''' </summary>
    ''' <returns></returns>
    Public Overrides Function ToString() As String
      Return Text
    End Function

    ''' <summary>
    ''' Source text (substring) given start and length.
    ''' </summary>
    ''' <param name="start">character start index</param>
    ''' <param name="length">total number of characters</param>
    ''' <returns>subtext</returns>
    Public Overloads Function ToString(start As Integer, length As Integer) As String
      Return Text.Substring(start, length)
    End Function

    ''' <summary>
    ''' Source text (substring) given start and length.
    ''' </summary>
    ''' <param name="span">contains start and length</param>
    ''' <returns>subtext</returns>
    Public Overloads Function ToString(span As TextSpan) As String
      Return ToString(span.Start, span.Length)
    End Function

  End Class

End Namespace
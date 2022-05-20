Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.IO
Imports System.Text
Imports BASIC.CodeAnalysis.Text

Namespace BASIC.Tests.CodeAnalysis

  ' {
  '   const x = 100
  '   let x [=] 10
  ' }

  Friend NotInheritable Class AnnotatedText

    Sub New(text As String, spans As ImmutableArray(Of TextSpan))
      Me.Text = text
      Me.Spans = spans
    End Sub

    Public ReadOnly Property Text As String
    Public ReadOnly Property Spans As ImmutableArray(Of TextSpan)

    Public Shared Function Parse(text As String) As AnnotatedText

      text = Unindent(text)

      Dim textBuilder = New StringBuilder
      Dim spanBuilder = ImmutableArray.CreateBuilder(Of TextSpan)
      Dim startStack = New Stack(Of Integer)

      Dim position = 0
      For Each c In text
        If c = "["c Then
          startStack.Push(position)
        ElseIf c = "]" Then

          If startStack.Count = 0 Then
            Throw New ArgumentException("Too many ']' in text", NameOf(text))
          End If

          Dim start = startStack.Pop
          Dim e = position
          Dim span = TextSpan.FromBounds(start, e)
          spanBuilder.Add(span)

        Else
          position += 1
          textBuilder.Append(c)
        End If

      Next

      If startStack.Count <> 0 Then
        Throw New ArgumentException("Missing ']' in text", NameOf(text))
      End If

      Return New AnnotatedText(textBuilder.ToString, spanBuilder.ToImmutable)

    End Function

    Private Shared Function Unindent(text As String) As String
      Dim lines = UnindentLines(text)
      Return String.Join(Environment.NewLine, lines)
    End Function

    Public Shared Function UnindentLines(text As String) As String()
      Dim lines = New List(Of String)

      Using reader = New StringReader(text)
        'Dim line As String = Nothing
        Do
          Dim line = reader.ReadLine
          If line Is Nothing Then Exit Do
          lines.Add(line)
        Loop
      End Using

      Dim minIndentation = Integer.MaxValue
      For i = 0 To lines.Count - 1
        Dim line = lines(i)
        If line.Trim.Length = 0 Then lines(i) = String.Empty : Continue For
        Dim indentation = line.Length - line.TrimStart.Length
        minIndentation = Math.Min(minIndentation, indentation)
      Next

      For i = 0 To lines.Count - 1
        If lines(i).Length = 0 Then Continue For
        lines(i) = lines(i).Substring(minIndentation)
      Next

      While lines.Count > 0 AndAlso lines(0).Length = 0
        lines.RemoveAt(0)
      End While

      While lines.Count > 0 AndAlso lines(lines.Count - 1).Length = 0
        lines.RemoveAt(lines.Count - 1)
      End While

      Return lines.ToArray

    End Function

  End Class

End Namespace
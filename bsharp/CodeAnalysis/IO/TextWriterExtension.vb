Imports System.CodeDom.Compiler
Imports System.IO
Imports System.Runtime.CompilerServices
Imports Bsharp.CodeAnalysis.Syntax
Imports Bsharp.CodeAnalysis
Imports System.Console
Imports System.ConsoleColor
Imports Bsharp.CodeAnalysis.Text

Namespace Bsharp.IO

  Public Module TextWriterExtensions

    <Extension>
    Private Function IsConsole(writer As TextWriter) As Boolean

      If writer Is Console.Out Then
        Return Not Console.IsOutputRedirected
      End If

      If writer Is Console.Error Then
        Return Not Console.IsErrorRedirected And Not Console.IsOutputRedirected ' Color codes are always output to Console.Out
      End If

      If TypeOf writer Is IndentedTextWriter Then
        Dim iw = DirectCast(writer, IndentedTextWriter)
        If iw.InnerWriter.IsConsole() Then
          Return True
        End If
      End If

      Return False

    End Function

    <Extension>
    Private Sub SetForeground(writer As TextWriter, color As ConsoleColor)
      If writer.IsConsole() Then
        Console.ForegroundColor = color
      End If
    End Sub

    <Extension>
    Private Sub ResetColor(writer As TextWriter)
      If writer.IsConsole() Then
        Console.ResetColor()
      End If
    End Sub

    <Extension>
    Public Sub WriteKeyword(writer As TextWriter, kind As SyntaxKind)
      Dim text = SyntaxFacts.GetText(kind)
      Debug.Assert(kind.Is_Keyword AndAlso text IsNot Nothing)
      writer.WriteKeyword(text)
    End Sub

    <Extension>
    Public Sub WriteKeyword(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Blue)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteIdentifier(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.DarkYellow)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteNumber(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Cyan)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteString(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.Magenta)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteSpace(writer As TextWriter)
      writer.WritePunctuation(" ")
    End Sub


    <Extension>
    Public Sub WritePunctuation(writer As TextWriter, kind As SyntaxKind)
      Dim text = SyntaxFacts.GetText(kind)
      Debug.Assert(text IsNot Nothing)
      writer.WritePunctuation(text)
    End Sub

    <Extension>
    Public Sub WritePunctuation(writer As TextWriter, text As String)
      writer.SetForeground(ConsoleColor.DarkGray)
      writer.Write(text)
      writer.ResetColor()
    End Sub

    <Extension>
    Public Sub WriteDiagnostics(writer As TextWriter, diagnostics As IEnumerable(Of Diagnostic))

      Dim sepLine = False

      For Each diagnostic In diagnostics.Where(Function(diag) diag.Location.Text Is Nothing)
        Dim messageColor = If(diagnostic.IsWarning, DarkYellow, DarkRed)
        writer.SetForeground(messageColor)
        writer.WriteLine(diagnostic.Message)
        writer.ResetColor()
        sepLine = True
      Next

      ' We have errors, so don't try to evaluate (execute).
      For Each diagnostic In diagnostics.Where(Function(diag) diag.Location.Text IsNot Nothing).
                                         OrderBy(Function(diag) diag.Location.FileName).
                                         ThenBy(Function(diag) diag.Location.Span.Start).
                                         ThenBy(Function(diag) diag.Location.Span.Length)

        Dim text = diagnostic.Location.Text
        Dim filename = diagnostic.Location.FileName
        Dim startLine = diagnostic.Location.StartLine + 1
        Dim startCharacter = diagnostic.Location.StartCharacter + 1
        Dim endLine = diagnostic.Location.EndLine + 1
        Dim endCharacter = diagnostic.Location.EndCharacter + 1

        Dim span = diagnostic.Location.Span
        Dim lineIndex = text.GetLineIndex(span.Start)
        Dim line = text.Lines(lineIndex)

        Dim character = span.Start - line.Start + 1

        ' An extra line before for clarity...
        writer.WriteLine()

        Dim messageColor = If(diagnostic.IsWarning, DarkYellow, DarkRed)
        writer.SetForeground(messageColor)
        writer.Write($"{filename}({startLine},{startCharacter},{endLine},{endCharacter}): ")
        writer.WriteLine(diagnostic)
        writer.ResetColor()

        Dim prefixSpan = TextSpan.FromBounds(line.Start, span.Start)
        Dim suffixSpan = TextSpan.FromBounds(span.End, line.End)

        Dim prefix = text.ToString(prefixSpan)
        Dim er = text.ToString(span)
        Dim suffix = text.ToString(suffixSpan)

        ' Write the prefix in "normal" text...
        writer.Write($"    {prefix}")
        ' Write the error portion in red...
        writer.SetForeground(DarkRed)
        writer.Write(er)
        writer.ResetColor()
        ' Write the rest of the line.

        sepLine = True

      Next

      If sepLine Then
        ' An extra line at the end for clarity.
        writer.WriteLine()
      End If

    End Sub

  End Module

End Namespace
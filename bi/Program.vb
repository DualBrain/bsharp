Imports System.Text
Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Friend Module Program

  Friend Sub Main()

    Dim repl = New Basic.BasicRepl
    repl.Run()

    End

    Dim parseTreeVisible = False
    Dim boundTreeVisible = False
    Dim variables = New Dictionary(Of VariableSymbol, Object)
    Dim textBuilder = New StringBuilder
    Dim previous As Compilation = Nothing

    Do

      If textBuilder.Length = 0 Then
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write("> ")
      Else
        Console.ForegroundColor = ConsoleColor.DarkGray
        Console.Write(". ")
      End If
      Console.ResetColor()

      Dim input = Console.ReadLine
      Dim isBlank = String.IsNullOrWhiteSpace(input)

      If textBuilder.Length = 0 Then
        If isBlank Then
          Exit Do
        ElseIf input.StartsWith("$") Then
          Select Case input.ToLower
            Case "$showparsetree" : parseTreeVisible = True
            Case "$hideparsetree" : parseTreeVisible = False
            Case "$showboundtree" : boundTreeVisible = True
            Case "$hideboundtree" : boundTreeVisible = False
            Case "$cls" : Console.Clear()
            Case "$exit", "$system", "$quit" : Exit Do
            Case "$new" : parseTreeVisible = False : previous = Nothing 'variables.Clear()
            Case Else
              Console.WriteLine("Unknown meta command.")
          End Select
          Continue Do
        End If
      End If

      textBuilder.AppendLine(input)
      Dim text = textBuilder.ToString
      Dim tree = SyntaxTree.Parse(text)

      If Not isBlank AndAlso tree.Diagnostics.Any Then
        Continue Do
      End If

      Dim compilation = If(previous Is Nothing,
                           New Compilation(tree),
                           previous.ContinueWith(tree))

      'Dim binder = New Binder()
      'Dim boundExpression = binder.BindExpression(tree.Root)
      'Dim diagnostics = tree.Diagnostics.Concat(binder.Diagnostics).ToArray

      If parseTreeVisible Then tree.Root.WriteTo(Console.Out)
      If boundTreeVisible Then compilation.EmitTree(Console.Out)

      Dim result = compilation.Evaluate(variables)

      If Not result.Diagnostics.Any Then
        Console.ForegroundColor = ConsoleColor.Magenta
        Console.WriteLine(result.Value)
        Console.ResetColor()
        previous = compilation
      Else

        For Each diag In result.Diagnostics

          Dim lineIndex = tree.Text.GetLineIndex(diag.Span.Start)
          Dim line = tree.Text.Lines(lineIndex)
          Dim lineNumber = lineIndex + 1
          Dim character = diag.Span.Start - line.Start + 1

          Console.ForegroundColor = ConsoleColor.Red
          Console.WriteLine($"({lineNumber}, {character}): {diag}")
          Console.ResetColor()

          Dim prefixSpan = TextSpan.FromBounds(line.Start, diag.Span.Start)
          Dim suffixSpan = TextSpan.FromBounds(diag.Span.End, line.End)

          Dim prefix = tree.Text.ToString(prefixSpan)
          Dim er = tree.Text.ToString(diag.Span)
          Dim suffix = tree.Text.ToString(suffixSpan)

          Console.Write($"    {prefix}")

          Console.ForegroundColor = ConsoleColor.Red
          Console.Write($"{er}")
          Console.ResetColor()

          Console.WriteLine(suffix)

        Next

      End If

      textBuilder.Clear()

      'Dim lexer = New Lexer(line)

      'Do

      '  Dim token = lexer.NextToken

      '  If token.Kind = SyntaxKind.EndOfFileToken Then Exit Do

      '  Console.Write($"{token.Kind}: '{token.Text}'")

      '  If token.Value IsNot Nothing Then
      '    Console.Write($" {token.Value}")
      '  End If

      '  Console.WriteLine()

      'Loop

    Loop

  End Sub

End Module
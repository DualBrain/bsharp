﻿Imports Basic.CodeAnalysis
Imports Basic.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text

Namespace Basic

  Friend NotInheritable Class BasicRepl
    Inherits Repl

    Private m_previous As Compilation
    Private m_showParseTree As Boolean
    Private m_showBoundTree As Boolean

    Private ReadOnly m_variables As New Dictionary(Of VariableSymbol, Object)

    Protected Overrides Sub RenderLine(line As String)
      Dim tokens = SyntaxTree.ParseTokens(line)
      For Each token In tokens
        Dim isKeyword = token.Kind.ToString().EndsWith("Keyword")
        Dim isNumber = token.Kind = SyntaxKind.NumberToken
        If isKeyword Then
          Console.ForegroundColor = ConsoleColor.Blue
        ElseIf Not isNumber Then
          Console.ForegroundColor = ConsoleColor.DarkGray
        End If
        Console.Write(token.Text)
        Console.ResetColor()
      Next
    End Sub

    Protected Overrides Sub EvaluateMetaCommand(input As String)
      Select Case input
        Case "$showparsetree" : m_showParseTree = True
        Case "$hideparsetree" : m_showParseTree = False
        Case "$showboundtree" : m_showBoundTree = True
        Case "$hideboundtree" : m_showBoundTree = False
        Case "$cls" : Console.Clear()
        Case "$exit", "$system", "$quit" : m_exit = True
        Case "$new"
          m_previous = Nothing
          m_variables.Clear()
        Case Else
          MyBase.EvaluateMetaCommand(input)
      End Select
    End Sub

    Protected Overrides Function IsCompleteSubmission(text As String) As Boolean
      If String.IsNullOrEmpty(text) Then Return True
      Dim tree = SyntaxTree.Parse(text)
      'If tree.Diagnostics.Any() Then Return False
      If GetLastToken(tree.Root.Statement).IsMissing Then Return False
      Return True
    End Function

    Private Function GetLastToken(node As SyntaxNode) As SyntaxToken
      If TypeOf node Is SyntaxToken Then Return CType(node, SyntaxToken)
      ' A syntax node should always contain at least 1 token.
      Return GetLastToken(node.GetChildren.Last)
    End Function

    Protected Overrides Sub EvaluateSubmission(text As String)

      Dim tree = SyntaxTree.Parse(text)

      Dim compilation = If(m_previous Is Nothing, New Compilation(tree), m_previous.ContinueWith(tree))

      If m_showParseTree Then tree.Root.WriteTo(Console.Out)

      If m_showBoundTree Then compilation.EmitTree(Console.Out)

      Dim result = compilation.Evaluate(m_variables)

      If Not result.Diagnostics.Any() Then
        Console.ForegroundColor = ConsoleColor.Magenta
        Console.WriteLine(result.Value)
        Console.ResetColor()
        m_previous = compilation
      Else

        For Each diagnostic In result.Diagnostics

          Dim lineIndex = tree.Text.GetLineIndex(diagnostic.Span.Start)
          Dim line = tree.Text.Lines(lineIndex)
          Dim lineNumber = lineIndex + 1
          Dim character = diagnostic.Span.Start - line.Start + 1

          Console.WriteLine()

          Console.ForegroundColor = ConsoleColor.DarkRed
          Console.Write($"({lineNumber}, {character}): ")
          Console.WriteLine(diagnostic)
          Console.ResetColor()

          Dim prefixSpan = TextSpan.FromBounds(line.Start, diagnostic.Span.Start)
          Dim suffixSpan = TextSpan.FromBounds(diagnostic.Span.[End], line.[End])

          Dim prefix = tree.Text.ToString(prefixSpan)
          Dim [error] = tree.Text.ToString(diagnostic.Span)
          Dim suffix = tree.Text.ToString(suffixSpan)

          Console.Write("    ")
          Console.Write(prefix)

          Console.ForegroundColor = ConsoleColor.DarkRed
          Console.Write([error])
          Console.ResetColor()

          Console.Write(suffix)
          Console.WriteLine()

        Next

        Console.WriteLine()

      End If

    End Sub

  End Class

End Namespace
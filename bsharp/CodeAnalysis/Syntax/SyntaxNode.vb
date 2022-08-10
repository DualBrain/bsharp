'Imports System.Reflection
Imports System.Reflection
Imports Bsharp.CodeAnalysis.Text

Namespace Bsharp.CodeAnalysis.Syntax

  Public MustInherit Class SyntaxNode

    Friend Sub New(tree As SyntaxTree)
      SyntaxTree = tree
    End Sub

    Public ReadOnly Property SyntaxTree As SyntaxTree

    MustOverride ReadOnly Property Kind As SyntaxKind

    Public Overridable ReadOnly Property Span() As TextSpan
      Get
        Dim first = GetChildren.First.Span
        Dim last = GetChildren.Last.Span
        Return TextSpan.FromBounds(first.Start, last.End)
      End Get
    End Property

    Public Overridable ReadOnly Property FullSpan() As TextSpan
      Get
        Try
          Dim first = GetChildren.First
          Dim last = GetChildren.Last
          If first Is Nothing OrElse last Is Nothing Then
            Return TextSpan.FromBounds(0, 0)
          Else
            Return TextSpan.FromBounds(first.FullSpan.Start, last.FullSpan.End)
          End If
        Catch
          Return TextSpan.FromBounds(0, 0)
        End Try
      End Get
    End Property

    Public ReadOnly Property Location As TextLocation
      Get
        Return New TextLocation(SyntaxTree.Text, Span)
      End Get
    End Property

    'Public MustOverride Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

    Public Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

      Dim properties = [GetType].GetProperties(BindingFlags.Public Or BindingFlags.Instance)

      For Each prop In properties
        If GetType(SyntaxNode).IsAssignableFrom(prop.PropertyType) Then
          Dim child = TryCast(prop.GetValue(Me), SyntaxNode)
          If child IsNot Nothing Then Yield child
        ElseIf GetType(SeparatedSyntaxList).IsAssignableFrom(prop.PropertyType) Then
          Dim separatedSyntaxList = TryCast(prop.GetValue(Me), SeparatedSyntaxList)
          If separatedSyntaxList IsNot Nothing Then
            For Each child In separatedSyntaxList.GetWithSeparators
              If child IsNot Nothing Then Yield child
            Next
          End If
        ElseIf GetType(IEnumerable(Of SyntaxNode)).IsAssignableFrom(prop.PropertyType) Then
          Dim values = TryCast(prop.GetValue(Me), IEnumerable(Of SyntaxNode))
          For Each child In values
            If child IsNot Nothing Then Yield child
          Next
        End If
      Next

    End Function

    Public Function GetLastToken() As SyntaxToken
      If TypeOf Me Is SyntaxToken Then Return CType(Me, SyntaxToken)
      ' A syntax node should always contain at least 1 token.
      Return GetChildren.Last.GetLastToken
    End Function

    Public Sub WriteTo(writer As System.IO.TextWriter)
      PrettyPrint(writer, Me)
    End Sub

    'Private Shared Sub PrettyPrint(writer As System.IO.TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

    '  Dim isToConsole = (writer Is Console.Out)
    '  If node Is Nothing Then Return

    '  Dim marker = If(isLast, "└──", "├──")

    '  If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray
    '  Console.Write($"{indent}{marker}")
    '  If isToConsole Then Console.ForegroundColor = If(TypeOf node Is SyntaxToken, ConsoleColor.Blue, ConsoleColor.Cyan)
    '  Console.Write($"{node.Kind}")

    '  If isToConsole Then Console.ResetColor()

    '  Dim t = TryCast(node, SyntaxToken)
    '  If t IsNot Nothing Then Console.Write($" {t.Value}")
    '  Console.WriteLine()

    '  indent += If(isLast, "   ", "│  ")

    '  Dim lastChild = node.GetChildren.LastOrDefault

    '  For Each child In node.GetChildren
    '    PrettyPrint(writer, child, indent, child Is lastChild)
    '  Next

    'End Sub

    Private Shared Sub PrettyPrint(writer As System.IO.TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

      Dim isToConsole = writer Is Console.Out
      Dim token = TryCast(node, SyntaxToken)

      If token IsNot Nothing Then
        For Each trivia In token.LeadingTrivia

          If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray

          writer.Write(indent)
          writer.Write("├──")

          If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGreen

          writer.WriteLine($"L: {trivia.Kind}")

        Next
      End If

      Dim hasTrailingTrivia = token IsNot Nothing AndAlso token.TrailingTrivia.Any
      Dim tokenMarker = If(Not hasTrailingTrivia AndAlso isLast, "└──", "├──")

      If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray

      writer.Write(indent)
      writer.Write(tokenMarker)

      If isToConsole Then
        Console.ForegroundColor = If(TypeOf node Is SyntaxToken, ConsoleColor.Blue, ConsoleColor.Cyan)
      End If
      writer.Write($"{node.Kind}")

      If token IsNot Nothing AndAlso token.Value IsNot Nothing Then
        writer.Write(" ")
        writer.Write(token.Value)
      End If

      If isToConsole Then
        Console.ResetColor()
      End If

      writer.WriteLine()

      If token IsNot Nothing Then
        For Each trivia In token.TrailingTrivia

          Dim isLastTrailingTrivia = trivia Is token.TrailingTrivia.Last
          Dim triviaMarker = If(isLast AndAlso isLastTrailingTrivia, "└──", "├──")

          If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray

          writer.Write(indent)
          writer.Write(triviaMarker)

          If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGreen

          writer.WriteLine($"T: {trivia.Kind}")

        Next
      End If

      indent += If(isLast, "   ", "│  ")

      Dim lastChild = node.GetChildren.LastOrDefault

      For Each child In node.GetChildren
        PrettyPrint(writer, child, indent, child Is lastChild)
      Next

    End Sub

    Public Overrides Function ToString() As String
      Using writer = New System.IO.StringWriter
        WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace
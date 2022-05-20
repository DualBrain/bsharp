Imports System.Reflection
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Public MustInherit Class SyntaxNode

    MustOverride ReadOnly Property Kind As SyntaxKind

    Public Overridable ReadOnly Property Span() As TextSpan
      Get
        Dim first = GetChildren.First.Span
        Dim last = GetChildren.Last.Span
        Return TextSpan.FromBounds(first.Start, last.End)
      End Get
    End Property

    Public Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)

      Dim properties = [GetType].GetProperties(BindingFlags.Public Or BindingFlags.Instance)

      For Each prop In properties
        If GetType(SyntaxNode).IsAssignableFrom(prop.PropertyType) Then
          Dim child = TryCast(prop.GetValue(Me), SyntaxNode)
          If child IsNot Nothing Then Yield child
        ElseIf GetType(IEnumerable(Of SyntaxNode)).IsAssignableFrom(prop.PropertyType) Then
          Dim values = TryCast(prop.GetValue(Me), IEnumerable(Of SyntaxNode))
          For Each child In values
            If child IsNot Nothing Then Yield child
          Next
        End If
      Next

    End Function

    Public Sub WriteTo(writer As IO.TextWriter)
      PrettyPrint(writer, Me)
    End Sub

    Private Shared Sub PrettyPrint(writer As IO.TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)

      Dim isToConsole = (writer Is Console.Out)
      If node Is Nothing Then Return

      Dim marker = If(isLast, "└──", "├──")

      If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray
      Console.Write($"{indent}{marker}")
      If isToConsole Then Console.ForegroundColor = If(TypeOf node Is SyntaxToken, ConsoleColor.Blue, ConsoleColor.Cyan)
      Console.Write($"{node.Kind}")

      If isToConsole Then Console.ResetColor()

      Dim t = TryCast(node, SyntaxToken)
      If t IsNot Nothing Then Console.Write($" {t.Value}")
      Console.WriteLine()

      indent += If(isLast, "   ", "│  ")

      Dim lastChild = node.GetChildren.LastOrDefault

      For Each child In node.GetChildren
        PrettyPrint(writer, child, indent, child Is lastChild)
      Next

    End Sub

    Public Overrides Function ToString() As String
      Using writer = New IO.StringWriter
        WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace
Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Linq
Imports System.Reflection

Namespace Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundNode

    Public MustOverride ReadOnly Property Kind As BoundNodeKind

    'TODO: Consider having a "raw" bound tree view containing the following...

    'Public Iterator Function GetChildren() As IEnumerable(Of BoundNode)

    '  Dim properties = [GetType].GetProperties(BindingFlags.Public Or BindingFlags.Instance)

    '  For Each prop In properties
    '    If GetType(BoundNode).IsAssignableFrom(prop.PropertyType) Then
    '      Dim child = TryCast(prop.GetValue(Me), BoundNode)
    '      If child IsNot Nothing Then Yield child
    '    ElseIf GetType(IEnumerable(Of BoundNode)).IsAssignableFrom(prop.PropertyType) Then
    '      Dim values = TryCast(prop.GetValue(Me), IEnumerable(Of BoundNode))
    '      For Each child In values
    '        If child IsNot Nothing Then Yield child
    '      Next
    '    End If
    '  Next

    'End Function

    'Private Iterator Function GetProperties() As IEnumerable(Of (Name As String, Value As Object))
    '  Dim properties = [GetType].GetProperties(BindingFlags.Public Or BindingFlags.Instance)
    '  For Each prop In properties
    '    If prop.Name = NameOf(Kind) OrElse prop.Name = NameOf(BoundBinaryExpression.Op) Then Continue For
    '    If GetType(BoundNode).IsAssignableFrom(prop.PropertyType) OrElse GetType(IEnumerable(Of BoundNode)).IsAssignableFrom(prop.PropertyType) Then Continue For
    '    Dim value = prop.GetValue(Me)
    '    If value IsNot Nothing Then Yield (prop.Name, value)
    '  Next
    'End Function

    'Public Sub WriteTo(writer As System.IO.TextWriter)
    '  PrettyPrint(writer, Me)
    'End Sub

    'Private Shared Sub PrettyPrint(writer As System.IO.TextWriter, node As BoundNode, Optional indent As String = "", Optional isLast As Boolean = True)

    '  Dim isToConsole = (writer Is Console.Out)
    '  If node Is Nothing Then Return

    '  Dim marker = If(isLast, "└──", "├──")

    '  If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray
    '  Console.Write($"{indent}{marker}")

    '  If isToConsole Then Console.ForegroundColor = GetColor(node)
    '  Dim text = GetText(node)
    '  writer.Write($"{text}")

    '  Dim isFirstProperty = True

    '  For Each p In node.GetProperties
    '    If isFirstProperty Then
    '      isFirstProperty = False
    '    Else
    '      If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray
    '      writer.Write(",")
    '    End If
    '    writer.Write(" ")
    '    If isToConsole Then Console.ForegroundColor = ConsoleColor.Yellow
    '    writer.Write(p.Name)
    '    If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkGray
    '    writer.Write(" = ")
    '    If isToConsole Then Console.ForegroundColor = ConsoleColor.DarkYellow
    '    writer.Write(p.Value)
    '  Next

    '  If isToConsole Then Console.ResetColor()

    '  writer.WriteLine()

    '  indent += If(isLast, "   ", "│  ")

    '  Dim lastChild = node.GetChildren.LastOrDefault

    '  For Each child In node.GetChildren
    '    PrettyPrint(writer, child, indent, child Is lastChild)
    '  Next

    'End Sub

    'Private Shared Function GetText(node As BoundNode) As String
    '  If TypeOf node Is BoundBinaryExpression Then Return CType(node, BoundBinaryExpression).Op.Kind.ToString & "Expression"
    '  If TypeOf node Is BoundUnaryExpression Then Return CType(node, BoundUnaryExpression).Op.Kind.ToString & "Expression"
    '  Return node.Kind.ToString
    'End Function

    'Private Shared Function GetColor(node As BoundNode) As ConsoleColor
    '  If TypeOf node Is BoundExpression Then Return ConsoleColor.Blue
    '  If TypeOf node Is BoundStatement Then Return ConsoleColor.Cyan
    '  Return ConsoleColor.Yellow
    'End Function

    Public Overrides Function ToString() As String
      Using writer = New System.IO.StringWriter
        'WriteTo(writer)
        WriteTo(writer)
        Return writer.ToString
      End Using
    End Function

  End Class

End Namespace
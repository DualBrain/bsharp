Imports System.IO

Namespace Bsharp.CodeAnalysis.Symbols

  Public MustInherit Class Symbol

    ' TODO: Constructors should be internal
    Protected Friend Sub New(name As String)
      Me.Name = name
    End Sub

    Public MustOverride ReadOnly Property Kind As SymbolKind
    Public ReadOnly Property Name As String

    Public Sub WriteTo(writer As TextWriter)
      SymbolPrinter.WriteTo(Me, writer)
    End Sub

    Public Overrides Function ToString() As String
      Using writer = New StringWriter()
        WriteTo(writer)
        Return writer.ToString()
      End Using
    End Function

  End Class

End Namespace
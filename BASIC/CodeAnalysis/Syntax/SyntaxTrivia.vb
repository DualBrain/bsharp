Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SyntaxTrivia

    Public Sub New(syntaxTree As SyntaxTree, kind As SyntaxKind, position As Integer, text As String)
      Me.SyntaxTree = syntaxTree
      Me.Kind = kind
      Me.Position = position
      Me.Text = text
    End Sub

    Public ReadOnly Property SyntaxTree As SyntaxTree
    Public ReadOnly Property Kind As SyntaxKind
    Public ReadOnly Property Position As Integer
    Public ReadOnly Property Span As TextSpan
      Get
        Return New TextSpan(Position, If(Text?.Length, 0))
      End Get
    End Property
    Public ReadOnly Property Text As String

  End Class

End Namespace
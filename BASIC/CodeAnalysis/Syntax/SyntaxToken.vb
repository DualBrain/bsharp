Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SyntaxToken
    Inherits SyntaxNode

    Public Sub New(kind As SyntaxKind, position As Integer, text As String, value As Object)
      Me.Kind = kind
      Me.Position = position
      Me.Text = text
      Me.Value = value
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind
    Public ReadOnly Property Position As Integer
    Public ReadOnly Property Text As String
    Public ReadOnly Property Value As Object
    Public Overrides ReadOnly Property Span As TextSpan
      Get
        Return New TextSpan(Position, If(Text?.Length, 0))
      End Get
    End Property
    Public ReadOnly Property IsMissing As Boolean
      Get
        Return Text Is nothing
      End Get
    End Property

  End Class

End Namespace
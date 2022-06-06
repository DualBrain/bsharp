Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class SyntaxToken
    Inherits SyntaxNode

    Public Sub New(tree As SyntaxTree, kind As SyntaxKind, position As Integer, text As String, value As Object, leadingTrivia As ImmutableArray(Of SyntaxTrivia), trailingTrivia As ImmutableArray(Of SyntaxTrivia))
      MyBase.New(tree)
      Me.Kind = kind
      Me.Position = position
      Me.Text = text
      IsMissing = text Is Nothing
      Me.Value = value
      Me.LeadingTrivia = leadingTrivia
      Me.TrailingTrivia = trailingTrivia
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

    Public Overrides ReadOnly Property FullSpan As TextSpan
      Get
        Dim start = If(LeadingTrivia.Length = 0, Span.Start, LeadingTrivia.First.Span.Start)
        Dim [end] = If(TrailingTrivia.Length = 0, Span.End, TrailingTrivia.Last.Span.End)
        Return TextSpan.FromBounds(start, [end])
      End Get
    End Property

    Public ReadOnly Property LeadingTrivia As ImmutableArray(Of SyntaxTrivia)
    Public ReadOnly Property TrailingTrivia As ImmutableArray(Of SyntaxTrivia)

    'Public Overrides Function GetChildren() As IEnumerable(Of SyntaxNode)
    '  Return Array.Empty(Of SyntaxNode)
    'End Function

    ''' <summary>
    ''' A token is missing if it was inserted by the parser and doesn't appear in source
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property IsMissing As Boolean

  End Class

End Namespace
Namespace Basic.CodeAnalysis.Text

  Public Structure TextLocation

    Sub New(text As SourceText, span As TextSpan)
      Me.Text = text
      Me.Span = span
    End Sub

    Public ReadOnly Property Text As SourceText
    Public ReadOnly Property Span As TextSpan

    Public ReadOnly Property FileName As String
      Get
        Return Text.FileName
      End Get
    End Property

    Public ReadOnly Property StartLine As Integer
      Get
        Return Text.GetLineIndex(Span.Start)
      End Get
    End Property

    Public ReadOnly Property EndLine As Integer
      Get
        Return Text.GetLineIndex(Span.End)
      End Get
    End Property

    Public ReadOnly Property StartCharacter As Integer
      Get
        Return Span.Start - Text.Lines(StartLine).Start
      End Get
    End Property

    Public ReadOnly Property EndCharacter As Integer
      Get
        Return Span.End - Text.Lines(StartLine).Start
      End Get
    End Property

  End Structure

End Namespace
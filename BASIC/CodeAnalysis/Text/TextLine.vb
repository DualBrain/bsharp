Namespace Basic.CodeAnalysis.Text

  Public NotInheritable Class TextLine

    Public Sub New(text As SourceText, start As Integer, length As Integer, lengthIncludingLineBreak As Integer)
      Me.Text = text
      Me.Start = start
      Me.Length = length
      Me.LengthIncludingLineBreak = lengthIncludingLineBreak
    End Sub

    Public ReadOnly Property Text As SourceText
    Public ReadOnly Property Start As Integer
    Public ReadOnly Property Length As Integer
    Public ReadOnly Property LengthIncludingLineBreak As Integer

    Public ReadOnly Property [End] As Integer
      Get
        Return Start + Length
      End Get
    End Property

    Public ReadOnly Property Span As TextSpan
      Get
        Return New TextSpan(Start, Length)
      End Get
    End Property

    Public ReadOnly Property SpanIncludingLineBreak As TextSpan
      Get
        Return New TextSpan(Start, LengthIncludingLineBreak)
      End Get
    End Property

    Public Overrides Function ToString() As String
      Return Text.ToString(Span)
    End Function

  End Class

End Namespace
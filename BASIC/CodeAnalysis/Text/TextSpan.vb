Namespace Basic.CodeAnalysis.Text

  Public Structure TextSpan

    Public Sub New(start As Integer, length As Integer)
      Me.Start = start
      Me.Length = length
    End Sub

    Public ReadOnly Property Start As Integer
    Public ReadOnly Property Length As Integer
    Public ReadOnly Property [End] As Integer
      Get
        Return Start + Length
      End Get
    End Property

    Public Shared Function FromBounds(start As Integer, [end] As Integer) As TextSpan
      Dim length = [end] - start
      Return New TextSpan(start, length)
    End Function

    Public Overrides Function ToString() As String
      Return $"{Start}...{[End]}"
    End Function

  End Structure

End Namespace
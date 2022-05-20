Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis

  Public NotInheritable Class Diagnostic

    Public Sub New(span As TextSpan, message As String)
      Me.Span = span
      Me.Message = message
    End Sub

    Public ReadOnly Property Span As TextSpan
    Public ReadOnly Property Message As String
    Public Overrides Function ToString() As String
      Return Message
    End Function

  End Class

End Namespace
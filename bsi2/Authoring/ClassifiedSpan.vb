Imports Bsharp.CodeAnalysis.Text

Namespace Bsharp.CodeAnalysis.Authoring

  Public NotInheritable Class ClassifiedSpan

    Public Sub New(span As TextSpan, classification As Classification)
      Me.Span = span
      Me.Classification = classification
    End Sub

    Public ReadOnly Property Span As TextSpan
    Public ReadOnly Property Classification As Classification

  End Class

End Namespace
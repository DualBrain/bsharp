Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundNameStatement
    Inherits BoundStatement

    Public Sub New(originalPath As BoundExpression, destinationPath As BoundExpression)
      Me.OriginalPath = originalPath
      Me.DestinationPath = destinationPath
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.NameStatement
    Public ReadOnly Property OriginalPath As BoundExpression
    Public ReadOnly Property DestinationPath As BoundExpression

  End Class

End Namespace
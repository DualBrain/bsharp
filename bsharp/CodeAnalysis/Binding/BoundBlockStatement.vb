Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBlockStatement
    Inherits BoundStatement

    Public Sub New(statements As ImmutableArray(Of BoundStatement))
      Me.Statements = statements
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BlockStatement
    Public ReadOnly Property Statements As ImmutableArray(Of BoundStatement)

  End Class

End Namespace
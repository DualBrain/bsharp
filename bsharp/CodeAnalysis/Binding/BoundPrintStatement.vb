Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPrintStatement
    Inherits BoundStatement

    Public Sub New(nodes As ImmutableArray(Of BoundNode))
      Me.Nodes = nodes
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PrintStatement
    Public ReadOnly Property Nodes As ImmutableArray(Of BoundNode)

  End Class

End Namespace
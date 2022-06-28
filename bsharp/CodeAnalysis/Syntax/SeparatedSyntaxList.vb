Imports System.Collections.Immutable

Namespace Bsharp.CodeAnalysis.Syntax

  Public MustInherit Class SeparatedSyntaxList

    Public MustOverride Function GetWithSeparators() As ImmutableArray(Of SyntaxNode)

  End Class

  Public NotInheritable Class SeparatedSyntaxList(Of T As SyntaxNode)
    Inherits SeparatedSyntaxList
    Implements IEnumerable(Of T)

    Private ReadOnly m_nodesAndSeparators As ImmutableArray(Of SyntaxNode)

    Sub New(nodesAndSeparators As ImmutableArray(Of SyntaxNode))
      m_nodesAndSeparators = nodesAndSeparators
    End Sub

    Public ReadOnly Property Count As Integer
      Get
        Return (m_nodesAndSeparators.Length + 1) \ 2
      End Get
    End Property

    Default Public ReadOnly Property Item(index As Integer) As T
      Get
        Return CType(m_nodesAndSeparators(index * 2), T)
      End Get
    End Property

    Public Function GetSeparator(index As Integer) As SyntaxToken
      If index < 0 OrElse index >= Count - 1 Then Throw New ArgumentOutOfRangeException(NameOf(index))
      Return CType(m_nodesAndSeparators(index * 2 + 1), SyntaxToken)
    End Function

    Public Overrides Function GetWithSeparators() As ImmutableArray(Of SyntaxNode)
      Return m_nodesAndSeparators
    End Function

    Public Iterator Function GetEnumerator() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
      For i = 0 To Count - 1
        Yield Me(i)
      Next
    End Function

    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
      Return GetEnumerator()
    End Function

  End Class

End Namespace
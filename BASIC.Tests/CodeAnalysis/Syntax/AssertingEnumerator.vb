Option Explicit On
Option Strict On
Option Infer On

Imports Xunit

Namespace Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class AssertingEnumerator
    Implements IDisposable

    Private ReadOnly m_enumerator As IEnumerator(Of SyntaxNode)
    Private m_hasErrors As Boolean

    Public Sub New(node As SyntaxNode)
      m_enumerator = Flatten(node).GetEnumerator
    End Sub

    Private ReadOnly Property MarkFailed As Boolean
      Get
        m_hasErrors = True
        Return False
      End Get
    End Property

    Public Sub Dispose() Implements IDisposable.Dispose
      If Not m_hasErrors Then Assert.False(m_enumerator.MoveNext)
      m_enumerator.Dispose()
    End Sub

    Private Shared Iterator Function Flatten(node As SyntaxNode) As IEnumerable(Of SyntaxNode)

      Dim stack = New Stack(Of SyntaxNode)
      stack.Push(node)

      While stack.Count > 0

        Dim n = stack.Pop
        Yield n

        If n IsNot Nothing AndAlso
           n.GetChildren.Any Then
          For Each child In n.GetChildren.Reverse
            If child?.Kind IsNot Nothing Then
              stack.Push(child)
            End If
          Next
        End If

      End While

    End Function

    Public Sub AssertNode(kind As SyntaxKind)
      Try
        Assert.True(m_enumerator.MoveNext)
        Dim current = m_enumerator.Current
        Assert.Equal(kind, current.Kind)
        Assert.IsNotType(Of SyntaxToken)(current)
      Catch When MarkFailed
        Throw
      End Try
    End Sub

    Public Sub AssertToken(kind As SyntaxKind, text As String)
      Try
        Assert.True(m_enumerator.MoveNext)
        Dim current = m_enumerator.Current
        Assert.Equal(kind, current?.Kind)
        Dim token = Assert.IsType(Of SyntaxToken)(current)
        Assert.Equal(text, token.Text)
      Catch When MarkFailed
        Throw
      End Try
    End Sub

  End Class

End Namespace
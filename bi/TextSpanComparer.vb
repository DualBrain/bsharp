Imports Basic.CodeAnalysis.Text

Namespace Basic

  Friend Class TextSpanComparer
    Implements IComparer(Of TextSpan)

    Public Function Compare(x As TextSpan, y As TextSpan) As Integer Implements IComparer(Of TextSpan).Compare
      Dim c = x.Start - y.Start
      If c = 0 Then c = x.Length - y.Length
      Return c
    End Function

  End Class

End Namespace
Imports BASIC.CodeAnalysis.Text
Imports Xunit

Namespace BASIC.CodeAnalysis.Syntax

  Public Class SourceTextTests

    <Theory>
    <InlineData(".", 1)>
    <InlineData(".{vbcrlf}
", 2)>
    <InlineData(".
.
", 3)>
    Public Sub SourceText_IncludesLastLine(text As String, expectedLineCount As Integer)
      Dim st = SourceText.From(text)
      Assert.Equal(expectedLineCount, st.Lines.Length)
    End Sub

  End Class

End Namespace
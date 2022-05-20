Imports Xunit

Namespace BASIC.CodeAnalysis.Syntax

  Public Class SyntaxFactTests

    <Theory>
    <MemberData(NameOf(GetSyntaxKindData))>
    Public Sub SyntaxFact_GetText_RoundTrips(kind As SyntaxKind)

      Dim text = SyntaxFacts.GetText(kind)
      If text Is Nothing Then Return
      Dim tokens = SyntaxTree.ParseTokens(text)
      Dim token = Assert.Single(tokens)
      Assert.Equal(kind, token.Kind)
      Assert.Equal(text, token.Text)

    End Sub

    Public Shared Iterator Function GetSyntaxKindData() As IEnumerable(Of Object())
      Dim kinds = [Enum].GetValues(GetType(SyntaxKind))
      For Each kind In kinds
        Yield {kind}
      Next
    End Function

  End Class

End Namespace
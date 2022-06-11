Imports System.Collections.Immutable
Imports Basic.CodeAnalysis.Text
Imports Xunit

Namespace Basic.CodeAnalysis.Syntax

  Public Class LexerTests

    <Fact>
    Public Sub Lexer_Lexes_UnterminatedString()

      Dim text = """text"
      Dim diagnostics As ImmutableArray(Of Diagnostic) = Nothing
      Dim tokens = SyntaxTree.ParseTokens(text, diagnostics)

      Dim token = Assert.[Single](tokens)
      Assert.Equal(SyntaxKind.StringToken, token.Kind)
      Assert.Equal(text, token.Text)

      Dim diagnostic = Assert.[Single](diagnostics)
      Assert.Equal(New TextSpan(0, 1), diagnostic.Location.Span)
      Assert.Equal("Unterminated string literal.", diagnostic.Message)

    End Sub

    <Fact>
    Sub Lexer_Covers_AllToken()

      Dim tokenKinds = [Enum].GetValues(GetType(SyntaxKind)).
                              Cast(Of SyntaxKind).
                              Where(Function(k) k.ToString.EndsWith("Keyword") OrElse
                                                         k.ToString.EndsWith("Token")).
                              ToList

      Dim testedTokenKinds = GetTokens.Concat(GetSeparators).Select(Function(t) t.Kind)
      Dim untestedTokenKinds = New SortedSet(Of SyntaxKind)(tokenKinds)
      untestedTokenKinds.Remove(SyntaxKind.BadToken)
      untestedTokenKinds.Remove(SyntaxKind.EndOfFileToken)
      untestedTokenKinds.ExceptWith(testedTokenKinds)

      Assert.Empty(untestedTokenKinds)

    End Sub

    <Theory>
    <MemberData(NameOf(GetTokensData))>
    Sub Lexer_Lexes_Token(kind As SyntaxKind, text As String)
      Dim tokens = SyntaxTree.ParseTokens(text)
      Dim token = Assert.Single(tokens)
      Assert.Equal(kind, token.Kind)
      Assert.Equal(text, token.Text)
    End Sub

    <Theory>
    <MemberData(NameOf(GetSeparatorsData))>
    Public Sub Lexer_Lexes_Separator(kind As SyntaxKind, text As String)
      Dim tokens = SyntaxTree.ParseTokens(text, includeEndOfFile:=True)
      Dim token = Assert.Single(tokens)
      Dim trivia = Assert.Single(token.LeadingTrivia)
      Assert.Equal(kind, trivia.Kind)
      Assert.Equal(text, trivia.Text)
    End Sub

    <Theory>
    <MemberData(NameOf(GetTokenPairsData))>
    Sub Lexer_Lexes_TokenPairs(t1Kind As SyntaxKind, t1Text As String,
                               t2Kind As SyntaxKind, t2Text As String)
      Dim text = t1Text & t2Text
      Dim tokens = SyntaxTree.ParseTokens(text).ToArray
      Assert.Equal(2, tokens.Length)
      Assert.Equal(t1Kind, tokens(0).Kind)
      Assert.Equal(t1Text, tokens(0).Text)
      Assert.Equal(t2Kind, tokens(1).Kind)
      Assert.Equal(t2Text, tokens(1).Text)
    End Sub

    <Theory>
    <MemberData(NameOf(GetTokenPairsWithSeparatorData))>
    Sub Lexer_Lexes_TokenPairs_WithSeparators(t1Kind As SyntaxKind, t1Text As String,
                                              separatorKind As SyntaxKind, separatorText As String,
                                              t2Kind As SyntaxKind, t2Text As String)
      Dim text = t1Text & separatorText & t2Text
      Dim tokens = SyntaxTree.ParseTokens(text).ToArray
      Assert.Equal(2, tokens.Length)
      Assert.Equal(t1Kind, tokens(0).Kind)
      Assert.Equal(t1Text, tokens(0).Text)
      Dim separator = Assert.Single(tokens(0).TrailingTrivia)
      Assert.Equal(separatorKind, separator.Kind)
      Assert.Equal(separatorText, separator.Text)
      Assert.Equal(t2Kind, tokens(1).Kind)
      Assert.Equal(t2Text, tokens(1).Text)
    End Sub

    <Theory>
    <InlineData("foo")>
    <InlineData("foo42")>
    <InlineData("foo_42")>
    <InlineData("_foo")>
    Public Sub Lexer_Lexes_Identifiers(name As String)
      Dim tokens = SyntaxTree.ParseTokens(name).ToArray()
      Assert.Single(tokens)
      Dim token = tokens(0)
      Assert.Equal(SyntaxKind.IdentifierToken, token.Kind)
      Assert.Equal(name, token.Text)
    End Sub

    Public Shared Iterator Function GetTokensData() As IEnumerable(Of Object())
      For Each t In GetTokens() '.Concat(GetSeparators)
        Yield {t.Kind, t.Text}
      Next
    End Function

    Public Shared Iterator Function GetSeparatorsData() As IEnumerable(Of Object())
      For Each t In GetSeparators()
        Yield {t.Kind, t.Text}
      Next
    End Function

    Public Shared Iterator Function GetTokenPairsData() As IEnumerable(Of Object())
      For Each t In GetTokenPairs()
        Yield {t.T1Kind, t.T1Text, t.T2Kind, t.T2Text}
      Next
    End Function

    Public Shared Iterator Function GetTokenPairsWithSeparatorData() As IEnumerable(Of Object())
      For Each t In GetTokenPairsWithSeparator()
        Yield {t.T1Kind, t.T1Text, t.separatorKind, t.separatorText, t.T2Kind, t.T2Text}
      Next
    End Function

    Public Shared Function GetTokens() As IEnumerable(Of (Kind As SyntaxKind, Text As String))

      Dim fixedTokens = [Enum].GetValues(GetType(SyntaxKind)).
                               Cast(Of SyntaxKind).
                               [Select](Function(k) (kind:=k, text:=SyntaxFacts.GetText(k))).
                               Where(Function(t) t.text IsNot Nothing).
                               Cast(Of (SyntaxKind, String))

      Dim dynamicTokens As IEnumerable(Of (Kind As SyntaxKind, Text As String)) =
          {(SyntaxKind.NumberToken, "1"),
           (SyntaxKind.NumberToken, "123"),
           (SyntaxKind.IdentifierToken, "a"),
           (SyntaxKind.IdentifierToken, "abc"),
           (SyntaxKind.StringToken, """test"""),
           (SyntaxKind.StringToken, """te\st""")}

      Return fixedTokens.Concat(dynamicTokens)

    End Function

    Public Shared Function GetSeparators() As IEnumerable(Of (Kind As SyntaxKind, Text As String))
      Return {(SyntaxKind.WhiteSpaceTrivia, " "),
              (SyntaxKind.WhiteSpaceTrivia, "  "),
              (SyntaxKind.LineBreakTrivia, vbCr),
              (SyntaxKind.LineBreakTrivia, vbLf),
              (SyntaxKind.LineBreakTrivia, vbCrLf)} ',
      '((SyntaxKind.MultiLineCommentTrivia, "/**/"))}
    End Function

    Public Shared Function RequiresSeparator(t1Kind As SyntaxKind, t2Kind As SyntaxKind) As Boolean

      Dim t1IsKeyword = t1Kind.ToString.EndsWith("Keyword")
      Dim t2IsKeyword = t2Kind.ToString.EndsWith("Keyword")

      If t1Kind = SyntaxKind.IdentifierToken AndAlso t2Kind = SyntaxKind.IdentifierToken Then Return True
      If t1IsKeyword AndAlso t2IsKeyword Then Return True
      If t1IsKeyword AndAlso t2Kind = SyntaxKind.IdentifierToken Then Return True
      If t1Kind = SyntaxKind.IdentifierToken AndAlso t2IsKeyword Then Return True
      If t1IsKeyword AndAlso t2Kind = SyntaxKind.NumberToken Then Return True
      If t1Kind = SyntaxKind.StringToken AndAlso t2Kind = SyntaxKind.StringToken Then Return True
      If t1Kind = SyntaxKind.StringToken AndAlso t2Kind = SyntaxKind.NumberToken Then Return True
      If t1Kind = SyntaxKind.NumberToken AndAlso t2Kind = SyntaxKind.NumberToken Then Return True
      If t1Kind = SyntaxKind.IdentifierToken AndAlso t2Kind = SyntaxKind.NumberToken Then Return True
      If t1Kind = SyntaxKind.LessThanToken AndAlso t2Kind = SyntaxKind.GreaterThanToken Then Return True
      If t1Kind = SyntaxKind.LessThanToken AndAlso t2Kind = SyntaxKind.GreaterThanEqualToken Then Return True
      If t1Kind = SyntaxKind.EqualToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.PlusToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.MinusToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.StarToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.SlashToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.BackslashToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.LessThanToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.GreaterThanToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      'If t1Kind = SyntaxKind.AmersandToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.HatToken AndAlso t2Kind = SyntaxKind.EqualToken Then Return True
      If t1Kind = SyntaxKind.SlashToken AndAlso t2Kind = SyntaxKind.SlashToken Then Return True
      If t1Kind = SyntaxKind.SlashToken AndAlso t2Kind = SyntaxKind.StarToken Then Return True
      If t1Kind = SyntaxKind.SlashToken AndAlso t2Kind = SyntaxKind.SingleLineCommentTrivia Then Return True
      'If t1Kind = SyntaxKind.SlashToken AndAlso t2Kind = SyntaxKind.MultiLineCommentTrivia Then Return True
      'If t2Kind = SyntaxKind.DollarToken Then Return True
      Return False
    End Function

    Public Shared Iterator Function GetTokenPairs() As IEnumerable(Of (T1Kind As SyntaxKind, T1Text As String, T2Kind As SyntaxKind, T2Text As String))
      For Each t1 In GetTokens()
        For Each t2 In GetTokens()
          If Not RequiresSeparator(t1.Kind, t2.Kind) Then
            Yield (t1.Kind, t1.Text, t2.Kind, t2.Text)
          End If
        Next
      Next
    End Function

    Public Shared Iterator Function GetTokenPairsWithSeparator() As IEnumerable(Of (T1Kind As SyntaxKind,
                                                                                    T1Text As String,
                                                                                    separatorKind As SyntaxKind,
                                                                                    separatorText As String,
                                                                                    T2Kind As SyntaxKind,
                                                                                    T2Text As String))
      For Each t1 In GetTokens()
        For Each t2 In GetTokens()
          If RequiresSeparator(t1.Kind, t2.Kind) Then
            For Each s In GetSeparators()
              If Not RequiresSeparator(t1.Kind, s.Kind) AndAlso Not RequiresSeparator(s.Kind, t2.Kind) Then
                Yield (t1.Kind, t1.Text, s.Kind, s.Text, t2.Kind, t2.Text)
              End If
            Next
          End If
        Next
      Next
    End Function

  End Class

End Namespace
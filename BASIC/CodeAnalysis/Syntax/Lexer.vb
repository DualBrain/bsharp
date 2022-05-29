Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_text As SourceText

    Private m_position As Integer

    Private m_start As Integer
    Private m_kind As SyntaxKind
    Private m_value As Object

    Public Sub New(text As SourceText)
      m_text = text
    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag
      Get
        Return m_diagnostics
      End Get
    End Property

    Private ReadOnly Property Current() As Char
      Get
        Return Peek(0)
      End Get
    End Property

    Private ReadOnly Property LookAhead() As Char
      Get
        Return Peek(1)
      End Get
    End Property

    Private ReadOnly Property Peek(offset As Integer) As Char
      Get
        Dim index = m_position + offset
        If index >= m_text.Length Then Return Chr(0)
        Return m_text(index)
      End Get
    End Property

    Public Function Lex() As SyntaxToken

      m_start = m_position
      m_kind = SyntaxKind.BadToken
      m_value = Nothing

      Select Case Current
        Case Chr(0) : m_kind = SyntaxKind.EndOfFileToken
        Case "+"c : m_kind = SyntaxKind.PlusToken : m_position += 1
        Case "-"c : m_kind = SyntaxKind.MinusToken : m_position += 1
        Case "*"c : m_kind = SyntaxKind.StarToken : m_position += 1
        Case "/"c : m_kind = SyntaxKind.SlashToken : m_position += 1
        Case "\"c : m_kind = SyntaxKind.BackslashToken : m_position += 1
        Case "^"c : m_kind = SyntaxKind.HatToken : m_position += 1
        Case "("c : m_kind = SyntaxKind.OpenParenToken : m_position += 1
        Case ")"c : m_kind = SyntaxKind.CloseParenToken : m_position += 1
        Case "{"c : m_kind = SyntaxKind.OpenBraceToken : m_position += 1
        Case "}"c : m_kind = SyntaxKind.CloseBraceToken : m_position += 1
        Case "|"c : m_kind = SyntaxKind.CloseParenToken : m_position += 1
        Case "="c : m_kind = SyntaxKind.EqualToken : m_position += 1
        Case ">"c ' > >=
          Select Case LookAhead
            Case "="c : m_kind = SyntaxKind.GreaterThanEqualToken : m_position += 2
            Case Else : m_kind = SyntaxKind.GreaterThanToken : m_position += 1
          End Select
        Case "<"c ' < <= <>
          Select Case LookAhead
            Case "="c : m_kind = SyntaxKind.LessThanEqualToken : m_position += 2
            Case ">"c : m_kind = SyntaxKind.LessThanGreaterThanToken : m_position += 2
            Case Else : m_kind = SyntaxKind.LessThanToken : m_position += 1
          End Select
        Case "."c : m_kind = SyntaxKind.PeriodToken : m_position += 1
        Case ","c : m_kind = SyntaxKind.CommaToken : m_position += 1
        Case ":"c : m_kind = SyntaxKind.ColonToken : m_position += 1
        Case ";"c : m_kind = SyntaxKind.SemicolonToken : m_position += 1
        Case "?"c : m_kind = SyntaxKind.QuestionToken : m_position += 1
        Case "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c
          ReadNumberToken()
        Case " "c, CChar(vbTab), CChar(vbCr), CChar(vbLf)
          ReadWhiteSpace()
        Case Else
          If Char.IsLetter(Current) Then
            ReadIdentifierOrKeyword()
          ElseIf Char.IsWhiteSpace(Current) Then
            ReadWhiteSpace()
          Else
            m_diagnostics.ReportBadCharacter(m_start, Current)
            m_position += 1
          End If
      End Select

      Dim length = m_position - m_start
      Dim text = SyntaxFacts.GetText(m_kind)
      If text Is Nothing Then text = m_text.ToString(m_start, length)
      Return New SyntaxToken(m_kind, m_start, text, m_value)

    End Function

    Private Sub ReadWhiteSpace()
      While Char.IsWhiteSpace(Current)
        m_position += 1
      End While
      m_kind = SyntaxKind.WhitespaceToken
    End Sub

    Private Sub ReadNumberToken()

      While Char.IsDigit(Current)
        m_position += 1
      End While
      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)

      Dim value As Object = Nothing
      Dim temp As Integer
      If Integer.TryParse(text, temp) Then
        value = temp
      Else
        m_diagnostics.ReportInvalidNumber(New TextSpan(m_start, length), text, GetType(Integer))
      End If

      m_value = value
      m_kind = SyntaxKind.NumberToken

    End Sub

    Private Sub ReadIdentifierOrKeyword()
      While Char.IsLetter(Current)
        m_position += 1
      End While
      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)
      m_kind = SyntaxFacts.GetKeywordKind(text)
    End Sub

  End Class

End Namespace
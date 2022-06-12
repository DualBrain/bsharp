Imports System.Collections.Immutable
Imports System.Text
Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_syntaxTree As SyntaxTree
    Private ReadOnly m_text As SourceText
    Private m_position As Integer

    Private m_start As Integer
    Private m_kind As SyntaxKind
    Private m_value As Object
    Private ReadOnly m_triviaBuilder As ImmutableArray(Of SyntaxTrivia).Builder = ImmutableArray.CreateBuilder(Of SyntaxTrivia)

    Public Sub New(tree As SyntaxTree)
      m_syntaxTree = tree
      m_text = tree.Text
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

      ' If at beginning of a line, see if 
      ' we have a whole number followed by nothing but whitespace/crlf.

      'If m_position = 0 Then
      '  ' beginning of file
      '  ReadLineNumber()
      'End If

      ReadTrivia(True)

      Dim leadingTrivia = m_triviaBuilder.ToImmutable
      Dim tokenStart = m_position

      ReadToken()

      Dim tokenKind = m_kind
      Dim tokenValue = m_value
      Dim tokenLength = m_position - m_start

      ReadTrivia(False)

      Dim trailingTrivia = m_triviaBuilder.ToImmutable

      Dim tokenText = SyntaxFacts.GetText(tokenKind)
      If tokenText Is Nothing Then
        tokenText = m_text.ToString(tokenStart, tokenLength)
      End If

      Return New SyntaxToken(m_syntaxTree, tokenKind, tokenStart, tokenText, tokenValue, leadingTrivia, trailingTrivia)

    End Function

    Private Sub ReadTrivia(leading As Boolean)

      m_triviaBuilder.Clear()

      Dim done = False

      While Not done

        m_start = m_position
        m_kind = SyntaxKind.BadToken
        m_value = Nothing

        Select Case Current
          Case ChrW(0)
            done = True
          Case "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c
            Dim line = m_text.GetLineIndex(m_position)
            If m_position = 0 OrElse
               m_text.Lines(line).Start = m_position Then
              While Char.IsDigit(Current)
                m_position += 1
              End While
              m_kind = SyntaxKind.LineNumberTrivia
            Else
              done = True
            End If
          Case "'"c
            ReadSingleLineComment()
          Case ChrW(10), ChrW(13)
            If Not leading Then done = True
            ReadLineBreak()
          Case " "c, ChrW(9) ' Short-circuit whitespace checking (common).
            ReadWhiteSpace()
          Case Else
            If Char.IsWhiteSpace(Current) Then
              ReadWhiteSpace()
            Else
              done = True
            End If
        End Select
        Dim length = m_position - m_start
        If length > 0 Then
          Dim text = m_text.ToString(m_start, length)
          Dim trivia = New SyntaxTrivia(m_syntaxTree, m_kind, m_start, text)
          m_triviaBuilder.Add(trivia)
        End If
      End While

    End Sub

    Private Sub ReadLineBreak()

      If Current = ChrW(13) AndAlso LookAhead = ChrW(10) Then
        m_position += 2
      Else
        m_position += 1
      End If

      m_kind = SyntaxKind.LineBreakTrivia

    End Sub

    Private Sub ReadWhiteSpace()

      Dim done = False
      While Not done
        Select Case Current
          Case ChrW(0), ChrW(10), ChrW(13)
            done = True
          Case Else
            If Not Char.IsWhiteSpace(Current) Then
              done = True
            Else
              m_position += 1
            End If
        End Select
      End While

      m_kind = SyntaxKind.WhiteSpaceTrivia

    End Sub

    Private Sub ReadSingleLineComment()

      m_position += 1

      Dim done = False
      While Not done
        Select Case Current
          Case ChrW(0), ChrW(13), ChrW(10)
            done = True
          Case Else
            m_position += 1
        End Select
      End While

      m_kind = SyntaxKind.SingleLineCommentTrivia

    End Sub

    Private Sub ReadToken()

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
        'Case "|"c : m_kind = SyntaxKind.PipeToken : m_position += 1
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
        'Case "$"c : m_kind = SyntaxKind.DollarToken : m_position += 1
        Case ChrW(34)
          ReadString()
        Case "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c
          ReadNumberToken()
          'Case " "c, CChar(vbTab), CChar(vbCr), CChar(vbLf)
          '  ReadWhiteSpace()
        Case "_"c
          ReadIdentifierOrKeyword()
        Case Else
          If Char.IsLetter(Current) OrElse Current = "$"c Then
            ReadIdentifierOrKeyword()
            'ElseIf Char.IsWhiteSpace(Current) Then
            '  ReadWhiteSpace()
          Else
            Dim span = New TextSpan(m_position, 1)
            Dim location = New TextLocation(m_text, span)
            m_diagnostics.ReportBadCharacter(location, Current)
            m_position += 1
          End If
      End Select

      'Dim length = m_position - m_start
      'Dim text = SyntaxFacts.GetText(m_kind)
      'If text Is Nothing Then text = m_text.ToString(m_start, length)
      'Return New SyntaxToken(m_kind, m_start, text, m_value)

    End Sub

    Private Sub ReadString()

      ' "Test "" dddd"

      ' skip the current quote
      m_position += 1

      Dim sb = New StringBuilder
      Dim done = False

      While Not done
        Select Case Current
          Case ChrW(0), ChrW(13), ChrW(10)
            Dim span = New TextSpan(m_start, 1)
            Dim location = New TextLocation(m_text, span)
            Diagnostics.ReportUnterminatedString(location)
            done = True
          Case """"c
            If LookAhead = """"c Then
              sb.Append(Current)
              m_position += 2
            Else
              m_position += 1
              done = True
            End If
          Case Else
            sb.Append(Current)
            m_position += 1
        End Select
      End While

      m_kind = SyntaxKind.StringToken
      m_value = sb.ToString

    End Sub

    'Private Sub ReadWhiteSpace()
    '  While Char.IsWhiteSpace(Current)
    '    m_position += 1
    '  End While
    '  m_kind = SyntaxKind.WhitespaceToken
    'End Sub

    Private Sub ReadNumberToken()

      While Char.IsDigit(Current)
        m_position += 1
      End While
      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)
      Dim value As Integer
      If Not Integer.TryParse(text, value) Then
        Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
        m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Integer)
      End If

      m_value = value
      m_kind = SyntaxKind.NumberToken

    End Sub

    Private Sub ReadIdentifierOrKeyword()
      'While Char.IsLetter(Current)
      '  m_position += 1
      'End While
      'If Current = "$"c Then
      '  m_position += 1
      'End If
      'Dim length = m_position - m_start
      'Dim text = m_text.ToString(m_start, length)
      'm_kind = SyntaxFacts.GetKeywordKind(text)

      While Char.IsLetterOrDigit(Current) OrElse Current = "_"c OrElse Current = "$"c
        m_position += 1
      End While

      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)

      If text = "end" AndAlso Current = " "c Then

        Dim currentPosition = m_position

        ' skip spaces...
        While Current = " "c
          m_position += 1
        End While

        Dim st = m_position

        While Char.IsLetter(Current)
          m_position += 1
        End While

        Dim l = m_position - st
        Dim possible = m_text.ToString(st, l)

        Select Case possible
          Case "function", "if"
            length = m_position - m_start
            text = m_text.ToString(m_start, length)
          Case Else
            m_position = currentPosition
        End Select

      End If

      m_kind = SyntaxFacts.GetKeywordKind(text)

    End Sub

  End Class

End Namespace
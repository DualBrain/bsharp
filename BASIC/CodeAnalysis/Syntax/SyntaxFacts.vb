' Presedence
' -------------------------
' 14 ()
' 13 ^
' 12 - (negation "unary")
' 11 */
' 10 \
' 09 MOD
' 08 +-
' 07 = > >= < <= <>
' 06 NOT
' 05 AND, AndAlso
' 04 OR, OrElse
' 03 XOR
' 02 EQV
' 01 IMP

'TODO: Consider additional operators...

'  << and >>?
'  +=, -=, *=, /=, \=, ^=
'  & and &=?
'  for ++ and --?

Imports System.Runtime.CompilerServices

Namespace Basic.CodeAnalysis.Syntax

  Public Module SyntaxFacts

    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer
      Select Case kind
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken : Return 12
        Case SyntaxKind.NotKeyword : Return 6
        Case Else
          Return 0
      End Select
    End Function

    Public Function GetBinaryOperatorPrecedence(kind As SyntaxKind) As Integer
      Select Case kind
        Case SyntaxKind.HatToken : Return 13
        Case SyntaxKind.StarToken, SyntaxKind.SlashToken : Return 11
        Case SyntaxKind.BackslashToken : Return 10
        Case SyntaxKind.ModKeyword : Return 9
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken : Return 8
        Case SyntaxKind.EqualToken, SyntaxKind.GreaterThanToken, SyntaxKind.GreaterThanEqualToken, SyntaxKind.LessThanToken, SyntaxKind.LessThanEqualToken, SyntaxKind.LessThanGreaterThanToken : Return 7
        Case SyntaxKind.AndKeyword, SyntaxKind.AndAlsoKeyword : Return 5
        Case SyntaxKind.OrKeyword, SyntaxKind.OrElseKeyword : Return 4
        Case SyntaxKind.XorKeyword : Return 3
        Case SyntaxKind.EqvKeyword : Return 2
        Case SyntaxKind.ImpKeyword : Return 1
        Case Else
          Return 0
      End Select
    End Function

    Public Iterator Function GetUnaryOperatorKinds() As IEnumerable(Of SyntaxKind)
      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())
      For Each kind In kinds
        If GetUnaryOperatorPrecedence(kind) > 0 Then
          Yield kind
        End If
      Next
    End Function

    Public Iterator Function GetBinaryOperatorKinds() As IEnumerable(Of SyntaxKind)
      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())
      For Each kind In kinds
        If GetBinaryOperatorPrecedence(kind) > 0 Then
          Yield kind
        End If
      Next
    End Function

    Public Function GetKeywordKind(text As String) As SyntaxKind

      Select Case text

        Case "end" : Return SyntaxKind.EndKeyword

        Case "if" : Return SyntaxKind.IfKeyword
        Case "then" : Return SyntaxKind.ThenKeyword
        Case "elseif" : Return SyntaxKind.ElseIfKeyword
        Case "else" : Return SyntaxKind.ElseKeyword

        Case "while" : Return SyntaxKind.WhileKeyword
        Case "wend" : Return SyntaxKind.WendKeyword

        Case "for" : Return SyntaxKind.ForKeyword
        Case "to" : Return SyntaxKind.ToKeyword
        Case "step" : Return SyntaxKind.StepKeyword
        Case "next" : Return SyntaxKind.NextKeyword

        Case "select" : Return SyntaxKind.SelectKeyword
        Case "case" : Return SyntaxKind.CaseKeyword
        Case "is" : Return SyntaxKind.IsKeyword

        Case "const" : Return SyntaxKind.ConstKeyword
        Case "dim" : Return SyntaxKind.DimKeyword
        Case "let" : Return SyntaxKind.LetKeyword
        Case "true" : Return SyntaxKind.TrueKeyword
        Case "false" : Return SyntaxKind.FalseKeyword
        Case "mod" : Return SyntaxKind.ModKeyword
        Case "not" : Return SyntaxKind.NotKeyword
        Case "and" : Return SyntaxKind.AndKeyword
        Case "andalso" : Return SyntaxKind.AndAlsoKeyword
        Case "or" : Return SyntaxKind.OrKeyword
        Case "orelse" : Return SyntaxKind.OrElseKeyword
        Case "xor" : Return SyntaxKind.XorKeyword
        Case "eqv" : Return SyntaxKind.EqvKeyword
        Case "imp" : Return SyntaxKind.ImpKeyword

        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function

    Public Function GetText(kind As SyntaxKind) As String

      Select Case kind

        Case SyntaxKind.EndKeyword : Return "end"

        Case SyntaxKind.WhileKeyword : Return "while"
        Case SyntaxKind.WendKeyword : Return "wend"

        Case SyntaxKind.ForKeyword : Return "for"
        Case SyntaxKind.ToKeyword : Return "to"
        Case SyntaxKind.StepKeyword : Return "step"
        Case SyntaxKind.NextKeyword : Return "next"

        Case SyntaxKind.SelectKeyword : Return "select"
        Case SyntaxKind.CaseKeyword : Return "case"
        Case SyntaxKind.IsKeyword : Return "is"

        Case SyntaxKind.IfKeyword : Return "if"
        Case SyntaxKind.ThenKeyword : Return "then"
        Case SyntaxKind.ElseIfKeyword : Return "elseif"
        Case SyntaxKind.ElseKeyword : Return "else"
        'Case SyntaxKind.EndIfCommand : Return "end If"

        Case SyntaxKind.ConstKeyword : Return "const"
        Case SyntaxKind.DimKeyword : Return "dim"
        Case SyntaxKind.LetKeyword : Return "let"

        Case SyntaxKind.TrueKeyword : Return "true"
        Case SyntaxKind.FalseKeyword : Return "false"

        Case SyntaxKind.ModKeyword : Return "mod"
        Case SyntaxKind.NotKeyword : Return "not"
        Case SyntaxKind.AndKeyword : Return "and"
        Case SyntaxKind.AndAlsoKeyword : Return "andalso"
        Case SyntaxKind.OrKeyword : Return "or"
        Case SyntaxKind.OrElseKeyword : Return "orelse"
        Case SyntaxKind.XorKeyword : Return "xor"
        Case SyntaxKind.EqvKeyword : Return "eqv"
        Case SyntaxKind.ImpKeyword : Return "imp"
        Case SyntaxKind.PlusToken : Return "+"
        Case SyntaxKind.MinusToken : Return "-"
        Case SyntaxKind.StarToken : Return "*"
        Case SyntaxKind.SlashToken : Return "/"
        Case SyntaxKind.BackslashToken : Return "\"
        Case SyntaxKind.HatToken : Return "^"
        Case SyntaxKind.OpenParenToken : Return "("
        Case SyntaxKind.CloseParenToken : Return ")"
        Case SyntaxKind.OpenBraceToken : Return "{"
        Case SyntaxKind.CloseBraceToken : Return "}"
        Case SyntaxKind.EqualToken : Return "="
        Case SyntaxKind.LessThanToken : Return "<"
        Case SyntaxKind.PeriodToken : Return "."
        Case SyntaxKind.ColonToken : Return ":"
        Case SyntaxKind.CommaToken : Return ","
        Case SyntaxKind.SemicolonToken : Return ";"
        Case SyntaxKind.QuestionToken : Return "?"
        Case SyntaxKind.GreaterThanEqualToken : Return ">="
        Case SyntaxKind.LessThanEqualToken : Return "<="
        Case SyntaxKind.LessThanGreaterThanToken : Return "<>"
        Case SyntaxKind.GreaterThanToken : Return ">"

        Case Else
          Return Nothing
      End Select

    End Function

    <Extension>
    Public Function Is_Keyword(kind As SyntaxKind) As Boolean
      Return kind.ToString.EndsWith("Keyword")
    End Function

  End Module

End Namespace
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

    <Extension()>
    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer
      Select Case kind
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken : Return 12
        Case SyntaxKind.NotKeyword : Return 6
        Case Else
          Return 0
      End Select
    End Function

    <Extension()>
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

      Select Case text.ToLower

        Case "and" : Return SyntaxKind.AndKeyword
        Case "andalso" : Return SyntaxKind.AndAlsoKeyword
        Case "as" : Return SyntaxKind.AsKeyword
        Case "const" : Return SyntaxKind.ConstKeyword
        Case "continue" : Return SyntaxKind.ContinueKeyword
        Case "dim" : Return SyntaxKind.DimKeyword
        Case "do" : Return SyntaxKind.DoKeyword
        Case "each" : Return SyntaxKind.EachKeyword
        Case "else" : Return SyntaxKind.ElseKeyword
        Case "elseif" : Return SyntaxKind.ElseIfKeyword
        Case "end" : Return SyntaxKind.EndKeyword
        Case "end function" : Return SyntaxKind.EndFunctionKeyword
        Case "end if" : Return SyntaxKind.EndIfKeyword
        Case "eqv" : Return SyntaxKind.EqvKeyword
        Case "exit" : Return SyntaxKind.ExitKeyword
        Case "false" : Return SyntaxKind.FalseKeyword
        Case "for" : Return SyntaxKind.ForKeyword
        Case "function" : Return SyntaxKind.FunctionKeyword
        Case "goto" : Return SyntaxKind.GotoKeyword
        Case "if" : Return SyntaxKind.IfKeyword
        Case "imp" : Return SyntaxKind.ImpKeyword
        Case "in" : Return SyntaxKind.InKeyword
        Case "let" : Return SyntaxKind.LetKeyword
        Case "loop" : Return SyntaxKind.LoopKeyword
        Case "mod" : Return SyntaxKind.ModKeyword
        Case "next" : Return SyntaxKind.NextKeyword
        Case "not" : Return SyntaxKind.NotKeyword
        Case "or" : Return SyntaxKind.OrKeyword
        Case "orelse" : Return SyntaxKind.OrElseKeyword
        Case "print" : Return SyntaxKind.PrintKeyword
        Case "return" : Return SyntaxKind.ReturnKeyword
        Case "spc" : Return SyntaxKind.SpcKeyword
        Case "step" : Return SyntaxKind.StepKeyword
        Case "tab" : Return SyntaxKind.TabKeyword
        Case "then" : Return SyntaxKind.ThenKeyword
        Case "to" : Return SyntaxKind.ToKeyword
        Case "true" : Return SyntaxKind.TrueKeyword
        Case "until" : Return SyntaxKind.UntilKeyword
        Case "wend" : Return SyntaxKind.WendKeyword
        Case "while" : Return SyntaxKind.WhileKeyword
        Case "xor" : Return SyntaxKind.XorKeyword

        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function

    Public Function GetText(kind As SyntaxKind) As String

      Select Case kind

        Case SyntaxKind.AndKeyword : Return "And"
        Case SyntaxKind.AndAlsoKeyword : Return "AndAlso"
        Case SyntaxKind.AsKeyword : Return "As"
        Case SyntaxKind.ConstKeyword : Return "Const"
        Case SyntaxKind.ContinueKeyword : Return "Continue"
        Case SyntaxKind.DimKeyword : Return "Dim"
        Case SyntaxKind.DoKeyword : Return "Do"
        Case SyntaxKind.EachKeyword : Return "Each"
        Case SyntaxKind.ElseKeyword : Return "Else"
        Case SyntaxKind.ElseIfKeyword : Return "ElseIf"
        Case SyntaxKind.EndKeyword : Return "End"
        Case SyntaxKind.EndFunctionKeyword : Return "End Function"
        Case SyntaxKind.EndIfKeyword : Return "End If"
        Case SyntaxKind.EqvKeyword : Return "Eqv"
        Case SyntaxKind.ExitKeyword : Return "Exit"
        Case SyntaxKind.FalseKeyword : Return "False"
        Case SyntaxKind.ForKeyword : Return "For"
        Case SyntaxKind.FunctionKeyword : Return "Function"
        Case SyntaxKind.GotoKeyword : Return "Goto"
        Case SyntaxKind.IfKeyword : Return "If"
        Case SyntaxKind.ImpKeyword : Return "Imp"
        Case SyntaxKind.InKeyword : Return "In"
        Case SyntaxKind.LetKeyword : Return "Let"
        Case SyntaxKind.LoopKeyword : Return "Loop"
        Case SyntaxKind.ModKeyword : Return "Mod"
        Case SyntaxKind.NextKeyword : Return "Next"
        Case SyntaxKind.NotKeyword : Return "Not"
        Case SyntaxKind.OrKeyword : Return "Or"
        Case SyntaxKind.OrElseKeyword : Return "OrElse"
        Case SyntaxKind.PrintKeyword : Return "Print"
        Case SyntaxKind.ReturnKeyword : Return "Return"
        Case SyntaxKind.SpcKeyword : Return "Spc"
        Case SyntaxKind.StepKeyword : Return "Step"
        Case SyntaxKind.TabKeyword : Return "Tab"
        Case SyntaxKind.ThenKeyword : Return "Then"
        Case SyntaxKind.ToKeyword : Return "To"
        Case SyntaxKind.TrueKeyword : Return "True"
        Case SyntaxKind.UntilKeyword : Return "Until"
        Case SyntaxKind.WendKeyword : Return "Wend"
        Case SyntaxKind.WhileKeyword : Return "While"
        Case SyntaxKind.XorKeyword : Return "Xor"

        Case SyntaxKind.PoundToken : Return "#"
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
    Public Function IsComment(kind As SyntaxKind) As Boolean
      Select Case kind
        Case SyntaxKind.SingleLineCommentTrivia
          Return True
        Case Else
          Return False
      End Select
    End Function

    <Extension>
    Public Function IsTrivia(kind As SyntaxKind) As Boolean
      Select Case kind
        Case SyntaxKind.LineNumberTrivia,
             SyntaxKind.SkippedTextTrivia,
             SyntaxKind.LineBreakTrivia,
             SyntaxKind.WhiteSpaceTrivia,
             SyntaxKind.SingleLineCommentTrivia
          Return True
        Case Else
          Return False
      End Select
    End Function

    <Extension>
    Public Function Is_Keyword(kind As SyntaxKind) As Boolean
      Return kind.ToString.EndsWith("Keyword")
    End Function

    <Extension>
    Public Function IsToken(kind As SyntaxKind) As Boolean
      Return Not kind.IsTrivia AndAlso
             (kind.Is_Keyword OrElse kind.ToString.EndsWith("Token"))
    End Function

  End Module

End Namespace
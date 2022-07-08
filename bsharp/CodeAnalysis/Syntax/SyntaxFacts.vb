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

Namespace Bsharp.CodeAnalysis.Syntax

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
        Case "any" : Return SyntaxKind.AnyKeyword
        Case "base" : Return SyntaxKind.BaseKeyword
        Case "chdir" : Return SyntaxKind.ChDirKeyword
        Case "clear" : Return SyntaxKind.ClearKeyword
        Case "cls" : Return SyntaxKind.ClsKeyword
        Case "color" : Return SyntaxKind.ColorKeyword
        Case "common" : Return SyntaxKind.CommonKeyword
        Case "const" : Return SyntaxKind.ConstKeyword
        Case "continue" : Return SyntaxKind.ContinueKeyword
        Case "data" : Return SyntaxKind.DataKeyword
        Case "def" : Return SyntaxKind.DefKeyword
        Case "defdbl" : Return SyntaxKind.DefDblKeyword
        Case "defsng" : Return SyntaxKind.DefSngKeyword
        Case "deflng" : Return SyntaxKind.DefLngKeyword
        Case "defint" : Return SyntaxKind.DefIntKeyword
        Case "defstr" : Return SyntaxKind.DefStrKeyword
        Case "dim" : Return SyntaxKind.DimKeyword
        Case "do" : Return SyntaxKind.DoKeyword
        Case "each" : Return SyntaxKind.EachKeyword
        Case "else" : Return SyntaxKind.ElseKeyword
        Case "elseif" : Return SyntaxKind.ElseIfKeyword
        Case "end" : Return SyntaxKind.EndKeyword
        Case "end def" : Return SyntaxKind.EndDefKeyword
        Case "end function" : Return SyntaxKind.EndFunctionKeyword
        Case "end if" : Return SyntaxKind.EndIfKeyword
        Case "end sub" : Return SyntaxKind.EndSubKeyword
        Case "end type" : Return SyntaxKind.EndTypeKeyword
        Case "eqv" : Return SyntaxKind.EqvKeyword
        Case "erase" : Return SyntaxKind.EraseKeyword
        Case "error" : Return SyntaxKind.ErrorKeyword
        Case "exit" : Return SyntaxKind.ExitKeyword
        'Case "exit def" : Return SyntaxKind.ExitDefKeyword
        'Case "exit function" : Return SyntaxKind.ExitFunctionKeyword
        'Case "exit sub" : Return SyntaxKind.ExitSubKeyword
        Case "false" : Return SyntaxKind.FalseKeyword
        Case "for" : Return SyntaxKind.ForKeyword
        Case "function" : Return SyntaxKind.FunctionKeyword
        Case "goto" : Return SyntaxKind.GotoKeyword
        Case "gosub" : Return SyntaxKind.GosubKeyword
        Case "if" : Return SyntaxKind.IfKeyword
        Case "imp" : Return SyntaxKind.ImpKeyword
        Case "in" : Return SyntaxKind.InKeyword
        Case "input" : Return SyntaxKind.InputKeyword
        Case "kill" : Return SyntaxKind.KillKeyword
        'Case "lbound" : Return SyntaxKind.LBoundKeyword
        Case "let" : Return SyntaxKind.LetKeyword
        'Case "line input" : Return SyntaxKind.LineInputKeyword
        Case "locate" : Return SyntaxKind.LocateKeyword
        Case "lock" : Return SyntaxKind.LockKeyword
        Case "loop" : Return SyntaxKind.LoopKeyword
        Case "mid$" : Return SyntaxKind.MidKeyword
        Case "mkdir" : Return SyntaxKind.MkDirKeyword
        Case "mod" : Return SyntaxKind.ModKeyword
        Case "name" : Return SyntaxKind.NameKeyword
        Case "next" : Return SyntaxKind.NextKeyword
        Case "not" : Return SyntaxKind.NotKeyword
        Case "option" : Return SyntaxKind.OptionKeyword
        Case "or" : Return SyntaxKind.OrKeyword
        Case "orelse" : Return SyntaxKind.OrElseKeyword
        Case "print" : Return SyntaxKind.PrintKeyword
        Case "random" : Return SyntaxKind.RandomKeyword
        Case "randomize" : Return SyntaxKind.RandomizeKeyword
        Case "read" : Return SyntaxKind.ReadKeyword
        Case "redim" : Return SyntaxKind.RedimKeyword
        Case "rem" : Return SyntaxKind.RemKeyword
        Case "reset" : Return SyntaxKind.ResetKeyword
        Case "restore" : Return SyntaxKind.RestoreKeyword
        Case "return" : Return SyntaxKind.ReturnKeyword
        Case "rmdir" : Return SyntaxKind.RmDirKeyword
        Case "shared" : Return SyntaxKind.SharedKeyword
        Case "sleep" : Return SyntaxKind.SleepKeyword
        Case "spc" : Return SyntaxKind.SpcKeyword
        Case "static" : Return SyntaxKind.StaticKeyword
        Case "step" : Return SyntaxKind.StepKeyword
        Case "stop" : Return SyntaxKind.StopKeyword
        Case "sub" : Return SyntaxKind.SubKeyword
        Case "swap" : Return SyntaxKind.SwapKeyword
        Case "system" : Return SyntaxKind.SystemKeyword
        Case "tab" : Return SyntaxKind.TabKeyword
        Case "then" : Return SyntaxKind.ThenKeyword
        Case "to" : Return SyntaxKind.ToKeyword
        Case "true" : Return SyntaxKind.TrueKeyword
        Case "type" : Return SyntaxKind.TypeKeyword
        'Case "ubound" : Return SyntaxKind.UBoundKeyword
        Case "unlock" : Return SyntaxKind.UnlockKeyword
        Case "until" : Return SyntaxKind.UntilKeyword
        Case "wend" : Return SyntaxKind.WendKeyword
        Case "while" : Return SyntaxKind.WhileKeyword
        Case "width" : Return SyntaxKind.WidthKeyword
        Case "window" : Return SyntaxKind.WindowKeyword
        Case "write" : Return SyntaxKind.WriteKeyword
        Case "xor" : Return SyntaxKind.XorKeyword

        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function

    Public Function GetText(kind As SyntaxKind) As String

      Select Case kind

        Case SyntaxKind.AndKeyword : Return "And"
        Case SyntaxKind.AndAlsoKeyword : Return "AndAlso"
        Case SyntaxKind.AnyKeyword : Return "Any"
        Case SyntaxKind.AsKeyword : Return "As"
        Case SyntaxKind.BaseKeyword : Return "Base"
        Case SyntaxKind.ChDirKeyword : Return "ChDir"
        Case SyntaxKind.ClearKeyword : Return "Clear"
        Case SyntaxKind.ClsKeyword : Return "Cls"
        Case SyntaxKind.ColorKeyword : Return "Color"
        Case SyntaxKind.CommonKeyword : Return "Common"
        Case SyntaxKind.ConstKeyword : Return "Const"
        Case SyntaxKind.ContinueKeyword : Return "Continue"
        Case SyntaxKind.DataKeyword : Return "Data"
        Case SyntaxKind.DefKeyword : Return "Def"
        Case SyntaxKind.DefDblKeyword : Return "DefDbl"
        Case SyntaxKind.DefSngKeyword : Return "DefSng"
        Case SyntaxKind.DefLngKeyword : Return "DefLng"
        Case SyntaxKind.DefIntKeyword : Return "DefInt"
        Case SyntaxKind.DefStrKeyword : Return "DefStr"
        Case SyntaxKind.DimKeyword : Return "Dim"
        Case SyntaxKind.DoKeyword : Return "Do"
        Case SyntaxKind.EachKeyword : Return "Each"
        Case SyntaxKind.ElseKeyword : Return "Else"
        Case SyntaxKind.ElseIfKeyword : Return "ElseIf"
        Case SyntaxKind.EndKeyword : Return "End"
        Case SyntaxKind.EndDefKeyword : Return "End Def"
        Case SyntaxKind.EndFunctionKeyword : Return "End Function"
        Case SyntaxKind.EndIfKeyword : Return "End If"
        Case SyntaxKind.EndSubKeyword : Return "End Sub"
        Case SyntaxKind.EndTypeKeyword : Return "End Type"
        Case SyntaxKind.EraseKeyword : Return "Erase"
        Case SyntaxKind.ErrorKeyword : Return "Error"
        Case SyntaxKind.EqvKeyword : Return "Eqv"
        Case SyntaxKind.ExitKeyword : Return "Exit"
        'Case SyntaxKind.ExitDefKeyword : Return "Exit Def"
        Case SyntaxKind.FalseKeyword : Return "False"
        Case SyntaxKind.ForKeyword : Return "For"
        Case SyntaxKind.FunctionKeyword : Return "Function"
        Case SyntaxKind.GotoKeyword : Return "Goto"
        Case SyntaxKind.GosubKeyword : Return "Gosub"
        Case SyntaxKind.IfKeyword : Return "If"
        Case SyntaxKind.ImpKeyword : Return "Imp"
        Case SyntaxKind.InputKeyword : Return "Input"
        Case SyntaxKind.InKeyword : Return "In"
        Case SyntaxKind.KillKeyword : Return "Kill"
        'Case SyntaxKind.LBound : Return "LBound"
        Case SyntaxKind.LetKeyword : Return "Let"
        'Case SyntaxKind.LineInputKeyword : Return "Line Input"
        Case SyntaxKind.LocateKeyword : Return "Locate"
        Case SyntaxKind.LockKeyword : Return "Lock"
        Case SyntaxKind.LoopKeyword : Return "Loop"
        Case SyntaxKind.MidKeyword : Return "Mid$"
        Case SyntaxKind.MkDirKeyword : Return "MkDir"
        Case SyntaxKind.ModKeyword : Return "Mod"
        Case SyntaxKind.NameKeyword : Return "Name"
        Case SyntaxKind.NextKeyword : Return "Next"
        Case SyntaxKind.NotKeyword : Return "Not"
        Case SyntaxKind.OptionKeyword : Return "Option"
        Case SyntaxKind.OrKeyword : Return "Or"
        Case SyntaxKind.OrElseKeyword : Return "OrElse"
        Case SyntaxKind.PrintKeyword : Return "Print"
        Case SyntaxKind.RandomKeyword : Return "Random"
        Case SyntaxKind.RandomizeKeyword : Return "Randomize"
        Case SyntaxKind.ReadKeyword : Return "Read"
        Case SyntaxKind.RedimKeyword : Return "Redim"
        Case SyntaxKind.RemKeyword : Return "Rem"
        Case SyntaxKind.ResetKeyword : Return "Reset"
        Case SyntaxKind.RestoreKeyword : Return "Restore"
        Case SyntaxKind.ReturnKeyword : Return "Return"
        Case SyntaxKind.RmDirKeyword : Return "RmDir"
        Case SyntaxKind.SharedKeyword : Return "Shared"
        Case SyntaxKind.SleepKeyword : Return "Sleep"
        Case SyntaxKind.SpcKeyword : Return "Spc"
        Case SyntaxKind.StaticKeyword : Return "Static"
        Case SyntaxKind.StepKeyword : Return "Step"
        Case SyntaxKind.StopKeyword : Return "Stop"
        Case SyntaxKind.SubKeyword : Return "Sub"
        Case SyntaxKind.SwapKeyword : Return "Swap"
        Case SyntaxKind.SystemKeyword : Return "System"
        Case SyntaxKind.TabKeyword : Return "Tab"
        Case SyntaxKind.ThenKeyword : Return "Then"
        Case SyntaxKind.ToKeyword : Return "To"
        Case SyntaxKind.TrueKeyword : Return "True"
        Case SyntaxKind.TypeKeyword : Return "Type"
        'Case SyntaxKind.UBoundKeyword : Return "UBound"
        Case SyntaxKind.UnlockKeyword : Return "Unlock"
        Case SyntaxKind.UntilKeyword : Return "Until"
        Case SyntaxKind.WendKeyword : Return "Wend"
        Case SyntaxKind.WhileKeyword : Return "While"
        Case SyntaxKind.WidthKeyword : Return "width"
        Case SyntaxKind.WindowKeyword : Return "window"
        Case SyntaxKind.WriteKeyword : Return "write"
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
        Case SyntaxKind.QuestionToken : Return "?"

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
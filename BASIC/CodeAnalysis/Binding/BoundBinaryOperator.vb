Imports Basic.CodeAnalysis.Symbols
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBinaryOperator

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, type As TypeSymbol)
      Me.New(syntaxKind, kind, type, type, type)
    End Sub

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, type As TypeSymbol, resultType As TypeSymbol)
      Me.New(syntaxKind, kind, type, type, resultType)
    End Sub

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, leftType As TypeSymbol, rightType As TypeSymbol, resultType As TypeSymbol)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.LeftType = leftType
      Me.RightType = rightType
      Me.Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundBinaryOperatorKind
    Public ReadOnly Property LeftType As TypeSymbol
    Public ReadOnly Property RightType As TypeSymbol
    Public ReadOnly Property Type As TypeSymbol

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


    Private Shared ReadOnly m_operators As BoundBinaryOperator() =
      {New BoundBinaryOperator(SyntaxKind.HatToken, BoundBinaryOperatorKind.Raise, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Boolean, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Boolean, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.LogicalAnd, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.AndAlsoKeyword, BoundBinaryOperatorKind.LogicalAndAlso, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.LogicalOr, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.OrElseKeyword, BoundBinaryOperatorKind.LogicalOrElse, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.LogicalXor, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqvKeyword, BoundBinaryOperatorKind.BitwiseEqv, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqvKeyword, BoundBinaryOperatorKind.LogicalEqv, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.ImpKeyword, BoundBinaryOperatorKind.LogicalImp, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.ImpKeyword, BoundBinaryOperatorKind.BitwiseImp, TypeSymbol.Boolean)}

    Public Shared Function Bind(SyntaxKind As SyntaxKind, leftType As TypeSymbol, rightType As TypeSymbol) As BoundBinaryOperator

      For Each op In m_operators
        If op.SyntaxKind = SyntaxKind AndAlso op.LeftType Is leftType AndAlso op.RightType Is rightType Then
          Return op
        End If
      Next

      Return Nothing

    End Function

  End Class

End Namespace
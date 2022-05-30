Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBinaryOperator

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, type As Type)
      Me.New(syntaxKind, kind, type, type, type)
    End Sub

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, type As Type, resultType As Type)
      Me.New(syntaxKind, kind, type, type, resultType)
    End Sub

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundBinaryOperatorKind, leftType As Type, rightType As Type, resultType As Type)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.LeftType = leftType
      Me.RightType = rightType
      Me.Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundBinaryOperatorKind
    Public ReadOnly Property LeftType As Type
    Public ReadOnly Property RightType As Type
    Public ReadOnly Property Type As Type

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
      {New BoundBinaryOperator(SyntaxKind.HatToken, BoundBinaryOperatorKind.Raise, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, GetType(Boolean), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, GetType(Boolean), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.LogicalAnd, GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.AndAlsoKeyword, BoundBinaryOperatorKind.LogicalAndAlso, GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.LogicalOr, GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.OrElseKeyword, BoundBinaryOperatorKind.LogicalOrElse, GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, GetType(Integer)),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.LogicalXor, GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.EqvKeyword, BoundBinaryOperatorKind.BitwiseEqv, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.EqvKeyword, BoundBinaryOperatorKind.LogicalEqv, GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.ImpKeyword, BoundBinaryOperatorKind.LogicalImp, GetType(Integer), GetType(Boolean)),
       New BoundBinaryOperator(SyntaxKind.ImpKeyword, BoundBinaryOperatorKind.BitwiseImp, GetType(Boolean))}

    Public Shared Function Bind(SyntaxKind As SyntaxKind, leftType As Type, rightType As Type) As BoundBinaryOperator

      For Each op In m_operators
        If op.SyntaxKind = SyntaxKind AndAlso op.LeftType = leftType AndAlso op.RightType = rightType Then
          Return op
        End If
      Next

      Return Nothing

    End Function

  End Class

End Namespace
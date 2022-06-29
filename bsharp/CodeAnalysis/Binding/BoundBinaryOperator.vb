Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis.Binding

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
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Decimal),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Double),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Single),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.StarToken, BoundBinaryOperatorKind.Multiplication, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Decimal),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Double),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Single),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.SlashToken, BoundBinaryOperatorKind.Division, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Decimal),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Double),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Single),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.BackslashToken, BoundBinaryOperatorKind.IntegerDivision, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.ModKeyword, BoundBinaryOperatorKind.ModOperation, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Decimal),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Double),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Single),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.PlusToken, BoundBinaryOperatorKind.Addition, TypeSymbol.String),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Decimal),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Double),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Single),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.MinusToken, BoundBinaryOperatorKind.Subtraction, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Decimal, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Double, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Single, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.ULong64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Long64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.ULong, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Long, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.UInteger, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.SByte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Byte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.Boolean, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.EqualToken, BoundBinaryOperatorKind.Equal, TypeSymbol.String, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Decimal, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Double, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Single, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.ULong64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Long64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.ULong, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Long, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.UInteger, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.SByte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.Byte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanToken, BoundBinaryOperatorKind.GreaterThan, TypeSymbol.String, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Decimal, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Double, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Single, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.ULong64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Long64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.ULong, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Long, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.UInteger, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.SByte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.Byte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.GreaterThanEqualToken, BoundBinaryOperatorKind.GreaterThanEqual, TypeSymbol.String, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Decimal, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Double, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Single, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.ULong64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Long64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.ULong, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Long, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.UInteger, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.SByte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.Byte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanToken, BoundBinaryOperatorKind.LessThan, TypeSymbol.String, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Decimal, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Double, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Single, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.ULong64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Long64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.ULong, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Long, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.UInteger, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.SByte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.Byte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanEqualToken, BoundBinaryOperatorKind.LessThanEqual, TypeSymbol.String, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Decimal, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Double, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Single, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.ULong64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Long64, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.ULong, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Long, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.UInteger, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Integer, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.SByte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Byte, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.Boolean, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.LessThanGreaterThanToken, BoundBinaryOperatorKind.NotEqual, TypeSymbol.String, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.LogicalAnd, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.AndKeyword, BoundBinaryOperatorKind.BitwiseAnd, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.AndAlsoKeyword, BoundBinaryOperatorKind.LogicalAndAlso, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.LogicalOr, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Byte),
       New BoundBinaryOperator(SyntaxKind.OrKeyword, BoundBinaryOperatorKind.BitwiseOr, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.OrElseKeyword, BoundBinaryOperatorKind.LogicalOrElse, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.LogicalXor, TypeSymbol.Boolean),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.ULong64),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Long64),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.ULong),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Long),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.UInteger),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Integer),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.SByte),
       New BoundBinaryOperator(SyntaxKind.XorKeyword, BoundBinaryOperatorKind.BitwiseXor, TypeSymbol.Byte),
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
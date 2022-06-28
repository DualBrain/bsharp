Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryOperator

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundUnaryOperatorKind, operandType As TypeSymbol)
      Me.New(syntaxKind, kind, operandType, operandType)
    End Sub

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundUnaryOperatorKind, operandType As TypeSymbol, resultType As TypeSymbol)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.OperandType = operandType
      Me.Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundUnaryOperatorKind
    Public ReadOnly Property OperandType As TypeSymbol
    Public ReadOnly Property Type As TypeSymbol

    Private Shared ReadOnly m_operators As BoundUnaryOperator() =
      {New BoundUnaryOperator(SyntaxKind.NotKeyword, BoundUnaryOperatorKind.BitwiseComplement, TypeSymbol.Integer),
       New BoundUnaryOperator(SyntaxKind.NotKeyword, BoundUnaryOperatorKind.LogicalNegation, TypeSymbol.Boolean),
       New BoundUnaryOperator(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, TypeSymbol.Integer),
       New BoundUnaryOperator(SyntaxKind.MinusToken, BoundUnaryOperatorKind.Negation, TypeSymbol.Integer)}

    Public Shared Function Bind(SyntaxKind As SyntaxKind, operandType As TypeSymbol) As BoundUnaryOperator
      For Each op In m_operators
        If op.SyntaxKind = SyntaxKind AndAlso op.OperandType Is operandType Then
          Return op
        End If
      Next
      Return Nothing
    End Function

  End Class

End Namespace
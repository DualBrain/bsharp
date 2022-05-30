Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryOperator

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundUnaryOperatorKind, operandType As Type)
      Me.New(syntaxKind, kind, operandType, operandType)
    End Sub

    Private Sub New(syntaxKind As SyntaxKind, kind As BoundUnaryOperatorKind, operandType As Type, resultType As Type)
      Me.SyntaxKind = syntaxKind
      Me.Kind = kind
      Me.OperandType = operandType
      Me.Type = resultType
    End Sub

    Public ReadOnly Property SyntaxKind As SyntaxKind
    Public ReadOnly Property Kind As BoundUnaryOperatorKind
    Public ReadOnly Property OperandType As Type
    Public ReadOnly Property Type As Type

    Private Shared ReadOnly m_operators As BoundUnaryOperator() =
      {New BoundUnaryOperator(SyntaxKind.NotKeyword, BoundUnaryOperatorKind.BitwiseComplement, GetType(Integer)),
       New BoundUnaryOperator(SyntaxKind.NotKeyword, BoundUnaryOperatorKind.LogicalNegation, GetType(Boolean)),
       New BoundUnaryOperator(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, GetType(Integer)),
       New BoundUnaryOperator(SyntaxKind.MinusToken, BoundUnaryOperatorKind.Negation, GetType(Integer))}

    Public Shared Function Bind(SyntaxKind As SyntaxKind, operandType As Type) As BoundUnaryOperator

      For Each op In m_operators
        If op.SyntaxKind = SyntaxKind AndAlso op.OperandType = operandType Then
          Return op
        End If
      Next

      Return Nothing

    End Function

  End Class

End Namespace
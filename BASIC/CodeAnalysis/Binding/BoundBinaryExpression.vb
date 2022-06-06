Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundBinaryExpression
    Inherits BoundExpression

    Public Sub New(left As BoundExpression, op As BoundBinaryOperator, right As BoundExpression)
      Me.Left = left
      Me.Op = op
      Me.Right = right
      ConstantValue = ConstantFolding.Fold(left, op, right)
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BinaryExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return Op.Type
      End Get
    End Property
    Public ReadOnly Property Left As BoundExpression
    Public ReadOnly Property Op As BoundBinaryOperator
    Public ReadOnly Property Right As BoundExpression
    Public Overrides ReadOnly Property ConstantValue As BoundConstant

  End Class

End Namespace
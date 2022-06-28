Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundUnaryExpression
    Inherits BoundExpression

    Public Sub New(op As BoundUnaryOperator, operand As BoundExpression)
      Me.Op = op
      Me.Operand = operand
      ConstantValue = ConstantFolding.ComputeConstant(op, operand)
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.UnaryExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
      Get
        Return Op.Type
      End Get
    End Property
    Public ReadOnly Property Op As BoundUnaryOperator
    Public ReadOnly Property Operand As BoundExpression
    Public Overrides ReadOnly Property ConstantValue As BoundConstant

  End Class

End Namespace
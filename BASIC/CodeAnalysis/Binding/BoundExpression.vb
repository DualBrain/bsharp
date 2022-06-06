﻿Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend MustInherit Class BoundExpression
    Inherits BoundNode

    Public MustOverride ReadOnly Property Type As TypeSymbol

    Public Overridable ReadOnly Property ConstantValue As BoundConstant

  End Class

End Namespace
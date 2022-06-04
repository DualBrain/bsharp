﻿Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class BoundErrorExpression
    Inherits BoundExpression

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ErrorExpression
    Public Overrides ReadOnly Property Type As TypeSymbol = TypeSymbol.Error

  End Class

End Namespace
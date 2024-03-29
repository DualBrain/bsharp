﻿Imports Bsharp.CodeAnalysis.Binding

Namespace Bsharp.CodeAnalysis.Symbols

  Public NotInheritable Class GlobalVariableSymbol
    Inherits VariableSymbol

    Friend Sub New(name As String, isReadOnly As Boolean, type As TypeSymbol, constant As BoundConstant)
      MyBase.New(name, isReadOnly, type, constant)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.GlobalVariable

  End Class

  Public NotInheritable Class GlobalArraySymbol
    Inherits VariableSymbol

    Friend Sub New(name As String, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression)
      MyBase.New(name, True, type, lower, upper)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.GlobalVariable

  End Class

End Namespace
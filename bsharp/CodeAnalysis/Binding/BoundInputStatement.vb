Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class BoundInputStatement
    Inherits BoundStatement

    Public Sub New(suppressCr As Boolean, suppressQuestionMark As Boolean, prompt As BoundExpression, variables As ImmutableArray(Of VariableSymbol))
      Me.SuppressCr = suppressCr
      Me.SuppressQuestionMark = suppressQuestionMark
      Me.PromptExpression = prompt
      Me.Variables = variables
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.InputStatement
    Public ReadOnly Property SuppressCr As Boolean
    Public ReadOnly Property SuppressQuestionMark As Boolean
    Public ReadOnly Property PromptExpression As BoundExpression
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)

  End Class

End Namespace
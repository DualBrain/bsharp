'Imports System.Collections.Immutable

'Namespace Basic.CodeAnalysis.Binding

'  Friend NotInheritable Class BoundSelectCaseStatement
'    Inherits BoundStatement

'    Public Sub New(test As BoundExpression, cases As ImmutableArray(Of BoundCaseStatement), elseStatement As BoundStatement)
'      Me.Test = test
'      Me.Cases = cases
'      Me.ElseStatement = elseStatement
'    End Sub

'    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.SelectCaseStatement
'    Public ReadOnly Property Test As BoundExpression
'    Public ReadOnly Property Cases As ImmutableArray(Of BoundCaseStatement)
'    Public ReadOnly Property ElseStatement As BoundStatement

'  End Class

'  Friend NotInheritable Class BoundCaseStatement
'    Inherits BoundStatement

'    Public Sub New(matches As ImmutableArray(Of BoundMatchStatement), statement As BoundStatement)
'      Me.Matches = matches
'      Me.Statement = statement
'    End Sub

'    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BoundCaseStatement
'    Public ReadOnly Property Matches As ImmutableArray(Of BoundMatchStatement)
'    Public ReadOnly Property Statement As BoundStatement

'  End Class

'  Friend NotInheritable Class BoundMatchStatement
'    Inherits BoundStatement

'    Public Sub New(comparison As String, expression As BoundExpression, expressionTo As BoundExpression)
'      Me.Comparison = comparison
'      Me.Expression = expression
'      Me.ExpressionTo = expressionTo
'    End Sub

'    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.BoundMatchStatement
'    Public ReadOnly Property Comparison As String
'    Public ReadOnly Property Expression As BoundExpression
'    Public ReadOnly Property ExpressionTo As BoundExpression

'  End Class

'End Namespace

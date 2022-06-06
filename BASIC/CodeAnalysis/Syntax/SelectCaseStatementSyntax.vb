'Imports System.Collections.Immutable

'Namespace Basic.CodeAnalysis.Syntax
'  Public NotInheritable Class SelectCaseStatementSyntax
'    Inherits StatementSyntax

'    Public Sub New(test As ExpressionSyntax,
'                   cases As ImmutableArray(Of CaseClauseSyntax),
'                   caseElseClause As CaseElseClauseSyntax)
'      Me.Test = test
'      Me.Cases = cases
'      Me.CaseElseClause = caseElseClause
'    End Sub

'    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.SelectCaseStatement
'    Public ReadOnly Property Test As ExpressionSyntax
'    Public ReadOnly Property Cases As ImmutableArray(Of CaseClauseSyntax)
'    Public ReadOnly Property CaseElseClause As CaseElseClauseSyntax

'  End Class

'  Public NotInheritable Class CaseClauseSyntax
'    Inherits SyntaxNode

'    Public Sub New(matches As ImmutableArray(Of CaseMatchExpressionSyntax), statement As StatementSyntax)
'      Me.Matches = matches
'      Me.Statement = statement
'    End Sub

'    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseClauseSyntax
'    Public ReadOnly Property Matches As ImmutableArray(Of CaseMatchExpressionSyntax)
'    Public ReadOnly Property Statement As StatementSyntax

'  End Class

'  Public NotInheritable Class CaseElseClauseSyntax
'    Inherits SyntaxNode

'    Public Sub New(statement As StatementSyntax)
'      Me.Statement = statement
'    End Sub

'    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseElseClauseSyntax
'    Public ReadOnly Property Statement As StatementSyntax

'  End Class

'  Public NotInheritable Class CaseMatchExpressionSyntax
'    Inherits SyntaxNode

'    Public Sub New(comparisonKind As SyntaxKind, expression As ExpressionSyntax, Optional expressionTo As ExpressionSyntax = Nothing)
'      Me.ComparisonKind = comparisonKind
'      Me.Expression = expression
'      Me.ExpressionTo = expressionTo
'    End Sub

'    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CaseMatchExpressionSyntax
'    Public ReadOnly Property ComparisonKind As SyntaxKind
'    Public ReadOnly Property Expression As ExpressionSyntax
'    Public ReadOnly Property ExpressionTo As ExpressionSyntax

'  End Class

'End Namespace
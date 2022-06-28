'Option Explicit On
'Option Strict On
'Option Infer On

'Imports System.Reflection
'Imports Bsharp.CodeAnalysis.Syntax

'Namespace Global.Basic.CodeAnalysis.Syntax

'  Partial Class AsClauseSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield AsKeyword
'      Yield Identifier
'    End Function

'  End Class

'  Partial Class AssignmentExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield IdentifierToken
'      Yield EqualToken
'      Yield Expression
'    End Function

'  End Class

'  Partial Class BinaryExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield Left
'      Yield OperatorToken
'      Yield Right
'    End Function

'  End Class

'  Partial Class BlockStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield OpenBraceToken
'      For Each child In Statements
'        Yield child
'      Next
'      Yield CloseBraceToken
'    End Function

'  End Class

'  Partial Class CallExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield Identifier
'      Yield OpenParenToken
'      For Each child In Arguments.GetWithSeparators
'        Yield child
'      Next
'      Yield CloseParenToken
'    End Function

'  End Class

'  Partial Class CompilationUnitSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      For Each child In Members
'        Yield child
'      Next
'      Yield EndOfFileToken
'    End Function

'  End Class

'  Partial Class ContinueStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ContinueKeyword
'      Yield ScopeKeyword
'    End Function

'  End Class

'  Partial Class DoUntilStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield DoKeyword
'      Yield Statements
'      Yield UntilClause
'      Yield LoopKeyword
'    End Function

'  End Class

'  Partial Class DoWhileStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield DoKeyword
'      Yield Statements
'      Yield WhileClause
'      Yield LoopKeyword
'    End Function

'  End Class

'  Partial Class ExitStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ExitKeyword
'      Yield ScopeKeyword
'    End Function

'  End Class

'  Partial Class ExpressionStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield Expression
'    End Function

'  End Class

'  Partial Class ForEachStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ForKeyword
'      Yield EachKeyword
'      Yield Value
'      Yield InKeyword
'      Yield Array
'      Yield Statements
'      Yield NextKeyword
'    End Function

'  End Class

'  Partial Class ForStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ForKeyword
'      Yield Identifier
'      Yield EqualToken
'      Yield StartValue
'      Yield ToKeyword
'      Yield EndValue
'      Yield StepKeyword
'      Yield Increment
'      Yield Statements
'      Yield NextKeyword
'    End Function

'  End Class

'  Partial Class FunctionDeclarationSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield FunctionKeyword
'      Yield Identifier
'      Yield OpenParenToken
'      For Each child In Parameters.GetWithSeparators
'        Yield child
'      Next
'      Yield CloseParenToken
'      Yield AsClause
'      Yield Statements
'      Yield EndKeyword
'      Yield ClosingKeyword
'    End Function

'  End Class

'  Partial Class GlobalStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield Statement
'    End Function

'  End Class

'  Partial Class IfStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield IfKeyword
'      Yield Expression
'      Yield ThenKeyword
'      Yield Statements
'    End Function

'  End Class

'  Partial Class ElseIfStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ElseIfKeyword
'      Yield Expression
'      Yield ThenKeyword
'      Yield Statements
'    End Function

'  End Class

'  Partial Class ElseStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ElseKeyword
'      Yield Statements
'    End Function

'  End Class

'  Partial Class MultiLineIfBlock

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield IfStatement
'      'Yield ElseIfStatements
'      Yield ElseStatement
'      Yield EndKeyword
'      Yield ClosingKeyword
'    End Function

'  End Class

'  Partial Class LiteralExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield LiteralToken
'    End Function

'  End Class

'  Partial Class NameExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield IdentifierToken
'    End Function

'  End Class

'  Partial Class ParameterSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield Identifier
'      Yield AsClause
'    End Function

'  End Class

'  Partial Class ParenExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield OpenParenToken
'      Yield Expression
'      Yield CloseParenToken
'    End Function

'  End Class

'  Partial Class ReturnStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ReturnKeyword
'      Yield Expression
'    End Function

'  End Class

'  Partial Class SingleLineIfStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield IfKeyword
'      Yield Expression
'      Yield ThenKeyword
'      Yield Statements
'      Yield ElseClause
'    End Function

'  End Class

'  Partial Class SingleLineElseClauseSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield ElseKeyword
'      Yield Statements
'    End Function

'  End Class

'  Partial Class UnaryExpressionSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield OperatorToken
'      Yield Operand
'    End Function

'  End Class

'  Partial Class UntilClauseSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield UntilKeyword
'      Yield Expression
'    End Function

'  End Class

'  Partial Class VariableDeclarationSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield Keyword
'      Yield Identifier
'      Yield AsClause
'      Yield EqualToken
'      Yield Initializer
'    End Function

'  End Class

'  Partial Class WhileClauseSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield WhileKeyword
'      Yield Expression
'    End Function

'  End Class

'  Partial Class WhileStatementSyntax

'    Public Overrides Iterator Function GetChildren() As IEnumerable(Of SyntaxNode)
'      Yield WhileKeyword
'      Yield Expression
'      Yield Statements
'      Yield WendKeyword
'    End Function

'  End Class

'End Namespace
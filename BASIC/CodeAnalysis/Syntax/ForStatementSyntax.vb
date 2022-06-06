Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class ForStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree,
                   forKeyword As SyntaxToken,
                   identifier As SyntaxToken,
                   equalToken As SyntaxToken,
                   startValue As ExpressionSyntax,
                   toKeyword As SyntaxToken,
                   endValue As ExpressionSyntax,
                   stepKeyword As SyntaxToken,
                   increment As ExpressionSyntax,
                   statements As StatementSyntax,
                   nextKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.ForKeyword = forKeyword
      Me.Identifier = identifier
      Me.EqualToken = equalToken
      Me.StartValue = startValue
      Me.ToKeyword = toKeyword
      Me.EndValue = endValue
      Me.StepKeyword = stepKeyword
      Me.Increment = increment
      Me.Statements = statements
      Me.NextKeyword = nextKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.ForStatement
    Public ReadOnly Property ForKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property StartValue As ExpressionSyntax
    Public ReadOnly Property ToKeyword As SyntaxToken
    Public ReadOnly Property EndValue As ExpressionSyntax
    Public ReadOnly Property StepKeyword As SyntaxToken
    Public ReadOnly Property Increment As ExpressionSyntax
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property NextKeyword As SyntaxToken

  End Class

End Namespace
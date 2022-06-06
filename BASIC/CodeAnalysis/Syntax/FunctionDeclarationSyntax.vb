Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class FunctionDeclarationSyntax
    Inherits MemberSyntax

    Public Sub New(tree As SyntaxTree, functionKeyword As SyntaxToken,
                   identifier As SyntaxToken,
                   openParenToken As SyntaxToken,
                   parameters As SeparatedSyntaxList(Of ParameterSyntax),
                   closeParenToken As SyntaxToken,
                   asClause As AsClauseSyntax,
                   statements As StatementSyntax,
                   endFunctionKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.FunctionKeyword = functionKeyword
      Me.Identifier = identifier
      Me.OpenParenToken = openParenToken
      Me.Parameters = parameters
      Me.CloseParenToken = closeParenToken
      Me.AsClause = asClause
      Me.Statements = statements
      Me.EndFunctionKeyword = endFunctionKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.FunctionDeclaration
    Public ReadOnly Property FunctionKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParenToken As SyntaxToken
    Public ReadOnly Property Parameters As SeparatedSyntaxList(Of ParameterSyntax)
    Public ReadOnly Property CloseParenToken As SyntaxToken
    Public ReadOnly Property AsClause As AsClauseSyntax
    Public ReadOnly Property Statements As StatementSyntax
    Public ReadOnly Property EndFunctionKeyword As SyntaxToken

  End Class

End Namespace
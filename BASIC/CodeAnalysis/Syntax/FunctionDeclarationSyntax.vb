Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class FunctionDeclarationSyntax
    Inherits MemberSyntax

    Public Sub New(functionKeyword As SyntaxToken,
                   identifier As SyntaxToken,
                   openParen As SyntaxToken,
                   parameters As SeparatedSyntaxList(Of ParameterSyntax),
                   closeParen As SyntaxToken,
                   type As TypeClauseSyntax,
                   body As StatementSyntax,
                   endKeyword As SyntaxToken,
                   closingKeyword As SyntaxToken)
      Me.FunctionKeyword = functionKeyword
      Me.Identifier = identifier
      Me.OpenParen = openParen
      Me.Parameters = parameters
      Me.CloseParen = closeParen
      Me.Type = type
      Me.Body = body
      Me.EndKeyword = endKeyword
      Me.ClosingKeyword = closingKeyword
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.FunctionDeclaration
    Public ReadOnly Property FunctionKeyword As SyntaxToken
    Public ReadOnly Property Identifier As SyntaxToken
    Public ReadOnly Property OpenParen As SyntaxToken
    Public ReadOnly Property Parameters As SeparatedSyntaxList(Of ParameterSyntax)
    Public ReadOnly Property CloseParen As SyntaxToken
    Public ReadOnly Property Type As TypeClauseSyntax
    Public ReadOnly Property Body As StatementSyntax
    Public ReadOnly Property EndKeyword As SyntaxToken
    Public ReadOnly Property ClosingKeyword As SyntaxToken

  End Class

End Namespace
Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class VariableDeclarationSyntax
    Inherits StatementSyntax

    ' Const *variable*[*suffix*][ As *type*] = *literal*[, *variable*[*suffix*][ As *type*] = *literal*]...
    ' Dim *variable*[*suffix*][ As *type*][ = *expression*][, *variable*[*suffix*][ As *type*][ = *expression*]]...
    ' Dim *variable*[*suffix*](*subscripts*)[ As *type*][, *variable*[*suffix*](*subscripts*)[ As *type*]]...

    Public Sub New(tree As SyntaxTree, keywordToken As SyntaxToken, identifierToken As SyntaxToken, asClause As AsClauseSyntax, equalToken As SyntaxToken, initializer As ExpressionSyntax)
      MyBase.New(tree)
      Me.KeywordToken = keywordToken
      Me.IdentifierToken = identifierToken
      Me.AsClause = asClause
      Me.EqualToken = equalToken
      Me.Initializer = initializer
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.VariableDeclarationStatement
    Public ReadOnly Property KeywordToken As SyntaxToken
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property AsClause As AsClauseSyntax
    Public ReadOnly Property EqualToken As SyntaxToken
    Public ReadOnly Property Initializer As ExpressionSyntax

  End Class

End Namespace
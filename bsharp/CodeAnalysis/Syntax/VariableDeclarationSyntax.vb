Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class VariableDeclarationSyntax
    Inherits StatementSyntax

    ' Const *variable*[*suffix*][ As *type*] = *literal*[, *variable*[*suffix*][ As *type*] = *literal*]...
    ' Dim *variable*[*suffix*][ As *type*][ = *expression*][, *variable*[*suffix*][ As *type*][ = *expression*]]...
    ' Dim *variable*[*suffix*][(*subscript*)][ As *type*][, *variable*[*suffix*](*subscripts*)[ As *type*]]...

    Public Sub New(tree As SyntaxTree, keywordToken As SyntaxToken, identifierToken As SyntaxToken, subscriptClause As SubscriptClauseSyntax, asClause As AsClauseSyntax, initClause As InitClauseSyntax)
      MyBase.New(tree)
      Me.KeywordToken = keywordToken
      Me.IdentifierToken = identifierToken
      Me.SubscriptClause = subscriptClause
      Me.AsClause = asClause
      Me.InitClause = initClause
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.VariableDeclarationStatement
    Public ReadOnly Property KeywordToken As SyntaxToken
    Public ReadOnly Property IdentifierToken As SyntaxToken
    Public ReadOnly Property SubscriptClause As SubscriptClauseSyntax
    Public ReadOnly Property AsClause As AsClauseSyntax
    Public ReadOnly Property InitClause As InitClauseSyntax

  End Class

End Namespace
Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class OptionStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, optionKeyword As SyntaxToken, baseKeyword As SyntaxToken, numberToken As SyntaxToken)
      MyBase.New(tree)
      Me.OptionKeyword = optionKeyword
      Me.BaseKeyword = baseKeyword
      Me.NumberToken = numberToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.OptionStatement
    Public ReadOnly Property OptionKeyword As SyntaxToken
    Public ReadOnly Property BaseKeyword As SyntaxToken
    Public ReadOnly Property NumberToken As SyntaxToken

  End Class

End Namespace
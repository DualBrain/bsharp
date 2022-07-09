Namespace Bsharp.CodeAnalysis.Syntax

  Partial Public NotInheritable Class ReturnGosubStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree, returnKeyword As SyntaxToken, targetToken As SyntaxToken)
      MyBase.New(tree)
      Me.ReturnKeyword = returnKeyword
      Me.TargetToken = targetToken
    End Sub

    Public Overrides ReadOnly Property Kind() As SyntaxKind = SyntaxKind.ReturnGosubStatement
    Public ReadOnly Property ReturnKeyword() As SyntaxToken
    Public ReadOnly Property TargetToken() As SyntaxToken

  End Class

End Namespace
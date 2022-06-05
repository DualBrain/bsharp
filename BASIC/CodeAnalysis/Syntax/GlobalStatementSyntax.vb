Namespace Basic.CodeAnalysis.Syntax

  Partial Public NotInheritable Class GlobalStatementSyntax
    Inherits MemberSyntax

    'tree As SyntaxTree, 
    Sub New(statement As StatementSyntax)
      'MyBase.New(tree)
      Me.Statement = statement
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.GlobalStatement
    Public ReadOnly Property Statement As StatementSyntax

  End Class

End Namespace
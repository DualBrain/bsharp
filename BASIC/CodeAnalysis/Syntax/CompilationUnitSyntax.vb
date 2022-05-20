Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class CompilationUnitSyntax
    Inherits SyntaxNode

    Public Sub New(statement As StatementSyntax, endOfFileToken As SyntaxToken)
      Me.Statement = statement
      Me.EndOfFileToken = endOfFileToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CompilationUnit
    Public ReadOnly Property Statement As StatementSyntax
    Public ReadOnly Property EndOfFileToken As SyntaxToken

  End Class

End Namespace
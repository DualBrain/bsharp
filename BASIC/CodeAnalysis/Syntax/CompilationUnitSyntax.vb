Imports System.Collections.Immutable

Namespace Basic.CodeAnalysis.Syntax

  Public NotInheritable Class CompilationUnitSyntax
    Inherits SyntaxNode

    'Public Sub New(statement As StatementSyntax, endOfFileToken As SyntaxToken)
    Public Sub New(members As ImmutableArray(Of MemberSyntax), endOfFileToken As SyntaxToken)
      'Me.Statement = Statement
      Me.Members = members
      Me.EndOfFileToken = endOfFileToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.CompilationUnit
    'Public ReadOnly Property Statement As StatementSyntax
    Public ReadOnly Property Members As ImmutableArray(Of MemberSyntax)
    Public ReadOnly Property EndOfFileToken As SyntaxToken

  End Class

End Namespace
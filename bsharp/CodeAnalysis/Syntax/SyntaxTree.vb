Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Text

Namespace Bsharp.CodeAnalysis.Syntax

  Public NotInheritable Class SyntaxTree

    Private Delegate Sub ParseHandler(tree As SyntaxTree,
                                      ByRef root As CompilationUnitSyntax,
                                      ByRef diagnostics As ImmutableArray(Of Diagnostic))

    Private Sub New(text As SourceText, handler As ParseHandler)

      Me.Text = text

      Dim root As CompilationUnitSyntax = Nothing
      Dim d As ImmutableArray(Of Diagnostic) = Nothing
      handler(Me, root, d)

      Me.Diagnostics = d
      Me.Root = root

    End Sub

    Public ReadOnly Property Text As SourceText
    Public ReadOnly Property Diagnostics As ImmutableArray(Of Diagnostic)
    Public ReadOnly Property Root As CompilationUnitSyntax

    Public Shared Function Load(fileName As String) As SyntaxTree
      Dim text = System.IO.File.ReadAllText(fileName)
      Dim source = SourceText.From(text, fileName)
      Return Parse(source)
    End Function

    Private Shared Sub Parse(tree As SyntaxTree, ByRef root As CompilationUnitSyntax, ByRef diagnostics As ImmutableArray(Of Diagnostic))
      Dim parser = New Parser(tree)
      root = parser.ParseCompilationUnit
      diagnostics = parser.Diagnostics.ToImmutableArray
    End Sub

    Public Shared Function Parse(text As String) As SyntaxTree
      Dim source = SourceText.From(text)
      Return Parse(source)
    End Function

    Public Shared Function Parse(text As SourceText) As SyntaxTree
      Return New SyntaxTree(text, AddressOf Parse)
    End Function

    Public Shared Function ParseTokens(text As String, Optional includeEndOfFile As Boolean = False) As ImmutableArray(Of SyntaxToken)
      Dim source = SourceText.From(text)
      Return ParseTokens(source, includeEndOfFile)
    End Function

    Public Shared Function ParseTokens(text As String, ByRef diagnostics As ImmutableArray(Of Diagnostic), Optional includeEndOfFile As Boolean = False) As ImmutableArray(Of SyntaxToken)
      Dim source = SourceText.From(text)
      Return ParseTokens(source, diagnostics, includeEndOfFile)
    End Function

    Public Shared Function ParseTokens(text As SourceText, Optional includeEndOfFile As Boolean = False) As ImmutableArray(Of SyntaxToken)
      Return ParseTokens(text, Nothing, includeEndOfFile)
    End Function

    ' I think this will work; Minsk handled this by leveraging C#'s local function capability.
    Private Shared ReadOnly m_parsedTokens As New List(Of SyntaxToken)
    Private Shared m_includeEndOfFile As Boolean

    Public Shared Function ParseTokens(text As SourceText, ByRef diagnostics As ImmutableArray(Of Diagnostic), Optional includeEndOfFile As Boolean = False) As ImmutableArray(Of SyntaxToken)
      m_includeEndOfFile = includeEndOfFile
      m_parsedTokens.Clear()
      ' ParseTokens local function was here....
      Dim st = New SyntaxTree(text, AddressOf ParseTokens_ParseTokens)
      diagnostics = st.Diagnostics '.ToImmutableArray
      Return m_parsedTokens.ToImmutableArray
    End Function

    Private Shared Sub ParseTokens_ParseTokens(st As SyntaxTree, ByRef root As CompilationUnitSyntax, ByRef d As ImmutableArray(Of Diagnostic))
      'root = Nothing
      Dim l = New Lexer(st)
      Do
        Dim token = l.Lex
        If token.Kind = SyntaxKind.EndOfFileToken Then
          root = New CompilationUnitSyntax(st, ImmutableArray(Of MemberSyntax).Empty, token)
        End If
        If token.Kind <> SyntaxKind.EndOfFileToken OrElse m_includeEndOfFile Then
          m_parsedTokens.Add(token)
        End If
        If token.Kind = SyntaxKind.EndOfFileToken Then Exit Do
      Loop
      d = l.Diagnostics.ToImmutableArray
    End Sub

  End Class

End Namespace
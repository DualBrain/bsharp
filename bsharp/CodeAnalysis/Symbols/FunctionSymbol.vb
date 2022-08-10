Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis.Symbols

  Public NotInheritable Class FunctionSymbol
    Inherits Symbol

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = Nothing
    End Sub

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, declaration As FunctionDeclarationSyntax)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = declaration
    End Sub

    Sub New(name As String, parameters As ImmutableArray(Of ParameterSymbol), type As TypeSymbol, declaration As DefDeclarationSyntax)
      MyBase.New(name)
      Me.Parameters = parameters
      Me.Type = type
      Me.Declaration = New FunctionDeclarationSyntax(declaration.SyntaxTree, declaration.DefKeyword, declaration.Identifier, declaration.OpenParenToken, declaration.Parameters, declaration.CloseParenToken, declaration.Parameters.First.AsClause, declaration.Statements, declaration.EndDefKeyword)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Function
    Public ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
    Public ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Declaration As FunctionDeclarationSyntax

  End Class

End Namespace
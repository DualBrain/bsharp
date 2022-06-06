Imports BASIC.CodeAnalysis.Syntax
Imports Basic.CodeAnalysis.Text
Imports System.Collections.Immutable

Imports Basic.CodeAnalysis.Syntax.SyntaxFacts

Namespace BASIC.CodeAnalysis.Authoring

  Public NotInheritable Class Classifier

    Public Shared Function Classify(tree As SyntaxTree, span As TextSpan) As ImmutableArray(Of ClassifiedSpan)
      Dim result = ImmutableArray.CreateBuilder(Of ClassifiedSpan)
      ClassifyNode(tree.Root, span, result)
      Return result.ToImmutable
    End Function

    Private Shared Sub ClassifyNode(node As SyntaxNode, span As TextSpan, result As ImmutableArray(Of ClassifiedSpan).Builder)
      If node Is Nothing Then Return
      Dim test As Boolean
      Try
        test = node.FullSpan.OverlapsWith(span)
      Catch
      End Try
      If Not test Then Return
      If TypeOf node Is SyntaxToken Then
        ClassifyToken(CType(node, SyntaxToken), span, result)
      End If
      For Each child In node.GetChildren
        ClassifyNode(child, span, result)
      Next
    End Sub

    Private Shared Sub ClassifyToken(token As SyntaxToken, span As TextSpan, result As ImmutableArray(Of ClassifiedSpan).Builder)
      For Each leadingTrivia In token.LeadingTrivia
        ClassifyTrivia(leadingTrivia, span, result)
      Next
      AddClassification(token.Kind, token.Span, span, result)
      For Each trailingTrivia In token.TrailingTrivia
        ClassifyTrivia(trailingTrivia, span, result)
      Next
    End Sub

    Private Shared Sub ClassifyTrivia(trivia As SyntaxTrivia, span As TextSpan, result As ImmutableArray(Of ClassifiedSpan).Builder)
      AddClassification(trivia.Kind, trivia.Span, span, result)
    End Sub

    Private Shared Sub AddClassification(elementKind As SyntaxKind, elementSpan As TextSpan, span As TextSpan, result As ImmutableArray(Of ClassifiedSpan).Builder)

      If Not elementSpan.OverlapsWith(span) Then Return

      Dim adjustedStart = Math.Max(elementSpan.Start, span.Start)
      Dim adjustedEnd = Math.Min(elementSpan.End, span.End)
      Dim adjustedSpan = TextSpan.FromBounds(adjustedStart, adjustedEnd)
      Dim classification = GetClassification(elementKind)

      Dim classifiedSpan = New ClassifiedSpan(adjustedSpan, classification)
      result.Add(classifiedSpan)

    End Sub

    Private Shared Function GetClassification(kind As SyntaxKind) As Classification

      Dim isKeyword = kind.Is_Keyword
      Dim isNumber = kind = SyntaxKind.NumberToken
      Dim isIdentifier = kind = SyntaxKind.IdentifierToken
      Dim isString = kind = SyntaxKind.StringToken
      Dim isComment = kind.IsComment

      If isKeyword Then
        Return Classification.Keyword
      ElseIf isIdentifier Then
        Return Classification.Identifier
      ElseIf isNumber Then
        Return Classification.Number
      ElseIf isString Then
        Return Classification.String
      ElseIf isComment Then
        Return Classification.Comment
      Else
        Return Classification.Text
      End If

    End Function

  End Class

End Namespace
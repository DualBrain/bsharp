Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Threading
Imports Basic.CodeAnalysis.Binding
Imports Basic.CodeAnalysis.Syntax

Namespace Basic.CodeAnalysis

  Public NotInheritable Class Compilation

    Private m_globalScope As BoundGlobalScope

    Public Sub New(syntax As SyntaxTree)
      MyClass.New(Nothing, syntax)
    End Sub

    Private Sub New(prev As Compilation, syntax As SyntaxTree)
      Previous = prev
      SyntaxTree = syntax
    End Sub

    Private ReadOnly Property GlobalScope As BoundGlobalScope
      Get
        If m_globalScope Is Nothing Then
          Dim gs = Binder.BindGlobalScope(Previous?.GlobalScope, SyntaxTree.Root)
          Interlocked.CompareExchange(m_globalScope, gs, Nothing)
        End If
        Return m_globalScope
      End Get
    End Property

    Public Function ContinueWith(syntaxTree As SyntaxTree) As Compilation
      Return New Compilation(Me, syntaxTree)
    End Function

    Public Function Evaluate(variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult
      Dim diagnostics = SyntaxTree.Diagnostics.Concat(GlobalScope.Diagnostics).ToImmutableArray
      If diagnostics.Any Then Return New EvaluationResult(diagnostics, Nothing)
      Dim evaluator = New Evaluator(GlobalScope.Statement, variables)
      Dim value = evaluator.Evaluate
      Return New EvaluationResult(ImmutableArray(Of Diagnostic).Empty, value)
    End Function

    Public Sub EmitTree(writer As IO.TextWriter)
      GlobalScope.Statement.WriteTo(writer)
    End Sub

    Public ReadOnly Property Previous As Compilation
    Public ReadOnly Property SyntaxTree As SyntaxTree

  End Class

End Namespace
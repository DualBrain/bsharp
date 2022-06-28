Imports System.Collections.Immutable
Imports System.IO
Imports System.Threading

Imports Bsharp.CodeAnalysis.Binding
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax

Namespace Bsharp.CodeAnalysis

  Public NotInheritable Class Compilation

    Private m_globalScope As BoundGlobalScope = Nothing

    'Public Sub New(syntax As SyntaxTree)
    '  MyClass.New(Nothing, syntax)
    'End Sub

    'Private Sub New(prev As Compilation, syntax As SyntaxTree)
    '  Previous = prev
    '  SyntaxTree = syntax
    'End Sub

    Private Sub New(isScript As Boolean,
                    previous As Compilation,
                    ParamArray syntaxTrees() As SyntaxTree)
      Me.IsScript = isScript
      Me.Previous = previous
      Me.SyntaxTrees = syntaxTrees.ToImmutableArray
    End Sub

    Public Shared Function Create(ParamArray syntaxTrees() As SyntaxTree) As Compilation
      Return New Compilation(False, Nothing, syntaxTrees)
    End Function

    Public Shared Function CreateScript(previous As Compilation, ParamArray syntaxTrees() As SyntaxTree) As Compilation
      Return New Compilation(True, previous, syntaxTrees)
    End Function

    Public ReadOnly Property IsScript As Boolean
    Public ReadOnly Property Previous As Compilation
    'Public ReadOnly Property SyntaxTree As SyntaxTree
    Public ReadOnly Property SyntaxTrees As ImmutableArray(Of SyntaxTree)
    Public ReadOnly Property MainFunction As FunctionSymbol
      Get
        Return GlobalScope.MainFunction
      End Get
    End Property
    Public ReadOnly Property Functions As ImmutableArray(Of FunctionSymbol)
      Get
        Return GlobalScope.Functions
      End Get
    End Property
    Public ReadOnly Property Variables As ImmutableArray(Of VariableSymbol)
      Get
        Return GlobalScope.Variables
      End Get
    End Property

    Private ReadOnly Property GlobalScope As BoundGlobalScope
      Get
        If m_globalScope Is Nothing Then
          Dim gs = Binder.BindGlobalScope(IsScript, Previous?.GlobalScope, SyntaxTrees) 'SyntaxTree.Root)
          Interlocked.CompareExchange(m_globalScope, gs, Nothing)
        End If
        Return m_globalScope
      End Get
    End Property

    Public Iterator Function GetSymbols() As IEnumerable(Of Symbol)

      Dim submission = Me
      Dim seenSymbolNames = New HashSet(Of String)

      Dim builtInFunctions = Symbols.BuiltinFunctions.GetAll().ToList()

      While submission IsNot Nothing

        'Dim bindingFlags = Reflection.BindingFlags.Static Or
        '                   Reflection.BindingFlags.Public Or
        '                   Reflection.BindingFlags.NonPublic
        'Dim builtInFunctions = GetType(BuiltinFunctions).
        '                       GetFields(bindingFlags).
        '                       Where(Function(fi) fi.FieldType = GetType(FunctionSymbol)).
        '                       Select(Function(fi) CType(fi.GetValue(Nothing), FunctionSymbol)).
        '                       ToList

        For Each f In submission.Functions
          If (seenSymbolNames.Add(f.Name)) Then
            Yield f
          End If
        Next

        For Each v In submission.Variables
          If (seenSymbolNames.Add(v.Name)) Then
            Yield v
          End If
        Next

        For Each builtIn In builtInFunctions
          If seenSymbolNames.Add(builtIn.Name) Then
            Yield builtIn
          End If
        Next

        submission = submission.Previous

      End While

    End Function

    'Public Function ContinueWith(syntaxTree As SyntaxTree) As Compilation
    '  Return New Compilation(Me, syntaxTree)
    'End Function

    Private Function GetProgram() As BoundProgram
      Dim previous = Me.Previous?.GetProgram
      Return Binder.BindProgram(IsScript, previous, GlobalScope)
    End Function

    'Public Function Evaluate(variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult

    '  Dim diagnostics = SyntaxTree.Diagnostics.Concat(GlobalScope.Diagnostics).ToImmutableArray
    '  If diagnostics.Any Then
    '    Return New EvaluationResult(diagnostics, Nothing)
    '  End If
    '  Dim program = Binder.BindProgram(GlobalScope)


    '  'Dim appPath = Environment.GetCommandLineArgs(0)
    '  'Dim appDirectory = System.IO.Path.GetDirectoryName(appPath)
    '  'Dim cfgPath = System.IO.Path.Combine(appDirectory, "cfg.dot")
    '  'Dim cfgStatement = If(Not program.Statement.Statements.Any AndAlso program.Functions.Any, program.Functions.Last.Value, program.Statement)
    '  'Dim cfg = ControlFlowGraph.Create(cfgStatement)
    '  'Using sw = New System.IO.StreamWriter(cfgPath)
    '  '  cfg.WriteTo(sw)
    '  'End Using

    '  If program.Diagnostics.Any Then Return New EvaluationResult(program.Diagnostics, Nothing)
    '  Dim evaluator = New Evaluator(program, variables)
    '  Dim value = Evaluator.Evaluate
    '  Return New EvaluationResult(ImmutableArray(Of Diagnostic).Empty, value)
    'End Function

    Public Function Evaluate(variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult

      If GlobalScope.Diagnostics.Any Then
        Return New EvaluationResult(GlobalScope.Diagnostics, Nothing)
      End If

      Dim program = GetProgram()

      If program.ErrorDiagnostics.Any Then
        Return New EvaluationResult(program.Diagnostics, Nothing)
      End If

      Dim evaluator = New Evaluator(program, variables)
      Dim value = evaluator.Evaluate

      Return New EvaluationResult(program.WarningDiagnostics, value)

    End Function

    'Public Sub EmitTree(writer As System.IO.TextWriter)

    '  'Dim statement = GetStatement()
    '  'statement.WriteTo(writer)

    '  Dim program = Binder.BindProgram(GlobalScope)

    '  'program.Statement.WriteTo(writer)
    '  If program.Statement.Statements.Any Then
    '    program.Statement.WriteTo(writer)
    '  Else
    '    For Each fb In program.Functions
    '      If Not GlobalScope.Functions.Contains(fb.Key) Then
    '        Continue For
    '      End If
    '      fb.Key.WriteTo(writer)
    '      fb.Value.WriteTo(writer)
    '    Next
    '  End If

    'End Sub

    Public Sub EmitTree(writer As TextWriter)

      If GlobalScope.MainFunction IsNot Nothing Then
        EmitTree(GlobalScope.MainFunction, writer)
      ElseIf GlobalScope.ScriptFunction IsNot Nothing Then
        EmitTree(GlobalScope.ScriptFunction, writer)
      End If

      'Dim program = GetProgram()
      'If program.Statement.Statements.Any() Then
      '  program.Statement.WriteTo(writer)
      'Else
      '  For Each functionBody In program.Functions
      '    If Not GlobalScope.Functions.Contains(functionBody.Key) Then
      '      Continue For
      '    End If
      '    functionBody.Key.WriteTo(writer)
      '    writer.WriteLine()
      '    functionBody.Value.WriteTo(writer)
      '  Next
      'End If
    End Sub

    Public Sub EmitTree(symbol As FunctionSymbol, writer As TextWriter)

      Dim program = GetProgram()
      Dim body As BoundBlockStatement = Nothing
      'If Not program.Functions.TryGetValue(symbol, body) Then
      '  Return
      'End If
      symbol.WriteTo(writer)
      writer.WriteLine()
      If Not program.Functions.TryGetValue(symbol, body) Then
        Return
      End If
      body.WriteTo(writer)

    End Sub

    'Private Function GetStatement() As BoundBlockStatement
    '  Dim result = GlobalScope.Statement
    '  Return Lowering.Lowerer.Lower(result)
    'End Function

    ' TODO: References should be part of the compilation, not arguments for Emit
    Public Function Emit(moduleName As String, references As String(), outputPath As String) As ImmutableArray(Of Diagnostic)

      Dim parseDiagnostics = SyntaxTrees.SelectMany(Function(st) st.Diagnostics)
      Dim diagnostics = parseDiagnostics.Concat(GlobalScope.Diagnostics).ToImmutableArray()
      Dim errorDiagnostics = diagnostics.Where(Function(d) d.IsError)
      If errorDiagnostics.Any Then
        Return diagnostics
      End If

      Dim program = GetProgram()

      If program.Diagnostics.Any Then
        Return program.Diagnostics
      End If

      Return Bsharp.CodeAnalysis.Emit.Emitter.Emit(program, moduleName, references, outputPath)

    End Function

  End Class

End Namespace
Imports System.IO
Imports Bsharp.CodeAnalysis
Imports Bsharp.CodeAnalysis.Syntax
Imports Bsharp.IO
Imports Mono.Options

Module Program

  Function Main(args As String()) As Integer

    Dim outputPath$ = Nothing
    Dim moduleName$ = Nothing
    Dim platform$ = Nothing
    Dim referencePaths = New List(Of String)
    Dim sourcePaths = New List(Of String)
    Dim helpRequested = False

    Dim options = New OptionSet From
    {
      "usage: bsc <source-paths> [options]",
      {"r=", "The {path} of an assembly to reference", Sub(v) referencePaths.Add(v)},
      {"o=", "The output {path} of the file to create", Sub(v) outputPath = v},
      {"m=", "The {name} of the file to create", Sub(v) moduleName = v},
      {"p=", "The {platform} type of output to create", Sub(v) platform = v},
      {"<>", Sub(v) sourcePaths.Add(v)},
      {"?|h|help", "Prints help", Sub(v) helpRequested = True}
    }

    options.Parse(args)

    Dim target = Bsharp.CodeAnalysis.Emit.TargetPlatform.MicrosoftItermediateLanguage
    Select Case platform?.ToLower
      Case "msil", "il", "cil"
      Case "vb", "visualbasic" : target = Bsharp.CodeAnalysis.Emit.TargetPlatform.MicrosoftVisualBasic
      Case "js", "javascript" : target = Bsharp.CodeAnalysis.Emit.TargetPlatform.Javascript
      Case Else
    End Select

    If helpRequested Then options.WriteOptionDescriptions(Console.Out) : Return 0

    If sourcePaths.Count = 0 Then Console.Error.WriteLine("error: need at least one source file.") : Return 1

    If outputPath Is Nothing Then
      Select Case target
        Case Emit.TargetPlatform.MicrosoftItermediateLanguage
          outputPath = Path.ChangeExtension(sourcePaths(0), ".exe")
        Case Emit.TargetPlatform.MicrosoftVisualBasic
          outputPath = Path.ChangeExtension(sourcePaths(0), ".vb")
        Case Emit.TargetPlatform.Javascript
          outputPath = Path.ChangeExtension(sourcePaths(0), ".html")
        Case Else
          Throw New NotImplementedException
      End Select
    End If

    If moduleName Is Nothing Then moduleName = Path.GetFileNameWithoutExtension(outputPath)

    Dim syntaxTrees = New List(Of SyntaxTree)
    Dim hasErrors = False

    For Each path In sourcePaths
      If Not System.IO.File.Exists(path) Then
        Console.Error.WriteLine($"error: file '{path}' doesn't exist.")
        hasErrors = True
        Continue For
      End If
      Dim tree = SyntaxTree.Load(path)
      syntaxTrees.Add(tree)
    Next

    For Each path In referencePaths
      If Not System.IO.File.Exists(path) Then
        Console.Error.WriteLine($"error: file '{path}' doesn't exist.")
        hasErrors = True
        Continue For
      End If
    Next

    If Debugger.IsAttached Then
      'TODO: For testing, hardcoding the mscorlib reference.
      If Not referencePaths.Any Then
        referencePaths.Add("C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.8\mscorlib.dll")
      End If
    End If

    If hasErrors Then Return 1

    Dim c = Compilation.Create(syntaxTrees.ToArray)
    Dim diagnostics = c.Emit(target, moduleName, referencePaths.ToArray, outputPath)

    If diagnostics.Any Then
      Console.Error.WriteDiagnostics(diagnostics)
      Return 1
    End If

    Return 0

  End Function

End Module
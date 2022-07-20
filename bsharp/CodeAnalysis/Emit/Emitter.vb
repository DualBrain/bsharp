Imports System.Collections.Immutable
Imports Bsharp.CodeAnalysis.Binding

Namespace Bsharp.CodeAnalysis.Emit

  Friend NotInheritable Class Emitter

    Public Shared Function Emit(target As TargetPlatform, program As BoundProgram, moduleName As String, references() As String, outputPath As String) As ImmutableArray(Of Diagnostic)

      If program.ErrorDiagnostics.Any Then Return program.Diagnostics

      Select Case target
        Case TargetPlatform.MicrosoftVisualBasic
          Dim emitter = New VbEmitter(moduleName, references)
          Return emitter.Emit(program, outputPath)
        Case TargetPlatform.Javascript
          Dim emitter = New JsEmitter(moduleName, references)
          Return emitter.Emit(program, outputPath)
        Case TargetPlatform.MicrosoftItermediateLanguage
          Return MsilEmitter.Emit(program, moduleName, references, outputPath)
        Case Else
          Throw New NotImplementedException
      End Select

    End Function

  End Class

End Namespace
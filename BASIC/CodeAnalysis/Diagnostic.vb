Imports Basic.CodeAnalysis.Text

Namespace Basic.CodeAnalysis

  Public NotInheritable Class Diagnostic

    Public Sub New(isError As Boolean, location As TextLocation, message As String)
      Me.IsError = isError
      Me.Location = location
      Me.Message = message
      IsWarning = Not isError
    End Sub

    Public ReadOnly Property IsError As Boolean
    Public ReadOnly Property Location As TextLocation
    Public ReadOnly Property Message As String
    Public ReadOnly Property IsWarning As Boolean
    Public Overrides Function ToString() As String
      Return Message
    End Function

    Public Shared Function [Error](location As TextLocation, message As String) As Diagnostic
      Return New Diagnostic(isError:=True, location:=location, message:=message)
    End Function

    Public Shared Function Warning(location As TextLocation, message As String) As Diagnostic
      Return New Diagnostic(isError:=False, location:=location, message:=message)
    End Function

  End Class

End Namespace
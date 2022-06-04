Imports Basic.CodeAnalysis.Symbols

Namespace Basic.CodeAnalysis.Binding

  Friend NotInheritable Class Conversion

    Public Shared ReadOnly None As New Conversion(False, False, False)
    Public Shared ReadOnly Identity As New Conversion(True, True, True)
    Public Shared ReadOnly Implicit As New Conversion(True, False, True)
    Public Shared ReadOnly Explicit As New Conversion(True, False, False)

    Sub New(exists As Boolean, isIdentity As Boolean, isImplicit As Boolean)
      Me.Exists = exists
      Me.IsIdentity = isIdentity
      Me.IsImplicit = isImplicit
    End Sub

    Public ReadOnly Property Exists As Boolean
    Public ReadOnly Property IsIdentity As Boolean
    Public ReadOnly Property IsImplicit As Boolean
    Public ReadOnly Property IsExplicit As Boolean
      Get
        Return Exists AndAlso Not IsImplicit
      End Get
    End Property

    Public Shared Function Classify([from] As TypeSymbol, [to] As TypeSymbol) As Conversion

      If [from] Is [to] Then
        Return Conversion.Identity
      End If

      If [from] IsNot TypeSymbol.Nothing AndAlso [to] Is TypeSymbol.Any Then
        Return Conversion.Implicit
      End If

      If [from] Is TypeSymbol.Any AndAlso [to] IsNot TypeSymbol.Nothing Then
        Return Conversion.Explicit
      End If

      If [from] Is TypeSymbol.Boolean OrElse [from] Is TypeSymbol.Integer Then
        If [to] Is TypeSymbol.String Then
          Return Conversion.Explicit
        End If
      End If

      If [from] Is TypeSymbol.String Then
        If [to] Is TypeSymbol.Boolean OrElse [to] Is TypeSymbol.Integer Then
          Return Conversion.Explicit
        End If
      End If

      Return Conversion.None

    End Function

  End Class

End Namespace
Namespace Bsharp.CodeAnalysis.Symbols

  Public NotInheritable Class TypeSymbol
    Inherits Symbol

    Public Enum Type
      [Error]
      Any
      Udt
      [Object]
      [DateTime]
      [Boolean]
      [Char]
      [String]
      [Nothing]
      [Decimal]
      [Double]
      [Single]
      [ULong64]
      [Long64]
      [ULong]
      [Long]
      [UInteger]
      [Integer]
      [SByte]
      [Byte]
    End Enum

    Public Shared Function TypeSymbolToType(ts As TypeSymbol) As Type

      If ts Is TypeSymbol.Error Then Return Type.Error
      If ts Is TypeSymbol.Any Then Return Type.Any
      If ts Is TypeSymbol.Udt Then Return Type.Udt
      If ts Is TypeSymbol.Object Then Return Type.Object
      If ts Is TypeSymbol.DateTime Then Return Type.DateTime
      If ts Is TypeSymbol.Boolean Then Return Type.Boolean
      If ts Is TypeSymbol.Char Then Return Type.Char
      If ts Is TypeSymbol.String Then Return Type.String
      If ts Is TypeSymbol.Nothing Then Return Type.Nothing
      If ts Is TypeSymbol.Decimal Then Return Type.Decimal
      If ts Is TypeSymbol.Double Then Return Type.Double
      If ts Is TypeSymbol.Single Then Return Type.Single
      If ts Is TypeSymbol.ULong64 Then Return Type.ULong64
      If ts Is TypeSymbol.Long64 Then Return Type.Long64
      If ts Is TypeSymbol.ULong Then Return Type.ULong
      If ts Is TypeSymbol.Long Then Return Type.Long
      If ts Is TypeSymbol.UInteger Then Return Type.UInteger
      If ts Is TypeSymbol.Integer Then Return Type.Integer
      If ts Is TypeSymbol.SByte Then Return Type.SByte
      If ts Is TypeSymbol.Byte Then Return Type.Byte

      Throw New NotImplementedException()

    End Function

    Public Shared ReadOnly [Error] As New TypeSymbol("?")

    Public Shared ReadOnly Any As New TypeSymbol("Any")
    Public Shared ReadOnly Udt As New TypeSymbol("Udt")
    Public Shared ReadOnly [Object] As New TypeSymbol("Object") ' Object
    Public Shared ReadOnly [DateTime] As New TypeSymbol("DateTime") ' Date/DateTime
    Public Shared ReadOnly [Boolean] As New TypeSymbol("Boolean")
    Public Shared ReadOnly [Char] As New TypeSymbol("Char") ' Char; 2 bytes
    Public Shared ReadOnly [String] As New TypeSymbol("String")
    Public Shared ReadOnly [Nothing] As New TypeSymbol("Nothing")

    Public Shared ReadOnly [Decimal] As New TypeSymbol("Decimal") ' Decimal; 16 bytes
    Public Shared ReadOnly [Double] As New TypeSymbol("Double") ' Double; 8 bytes
    Public Shared ReadOnly [Single] As New TypeSymbol("Single") ' Single; 4 bytes

    Public Shared ReadOnly [ULong64] As New TypeSymbol("ULong64") ' ULong/UInt64; 8 bytes
    Public Shared ReadOnly [Long64] As New TypeSymbol("Long64") ' Long/Int64; 8 bytes
    Public Shared ReadOnly [ULong] As New TypeSymbol("ULong") ' UInteger/UInt32; 4 bytes
    Public Shared ReadOnly [Long] As New TypeSymbol("Long") ' Integer/Int32; 4 bytes
    Public Shared ReadOnly [UInteger] As New TypeSymbol("UInteger") ' UShort/UInt16; 2 bytes
    Public Shared ReadOnly [Integer] As New TypeSymbol("Integer") ' Short/Int16; 2 bytes
    Public Shared ReadOnly [SByte] As New TypeSymbol("SByte") ' SByte; 1 byte
    Public Shared ReadOnly [Byte] As New TypeSymbol("Byte") ' Byte; 1 byte

    Private Sub New(name As String)
      MyBase.New(name)
    End Sub

    Public Overrides ReadOnly Property Kind As SymbolKind = SymbolKind.Type

  End Class

End Namespace
Imports System.Collections.Immutable
Imports System.Reflection

Namespace Basic.CodeAnalysis.Symbols
  Friend Module BuiltinFunctions

#If BASIC Then
    Public ReadOnly Hex As New FunctionSymbol("HEX$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly Oct As New FunctionSymbol("OCT$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly Asc As New FunctionSymbol("ASC", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Int)
    Public ReadOnly Chr As New FunctionSymbol("CHR$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly Instr As New FunctionSymbol("INSTR", ImmutableArray.Create(New ParameterSymbol("start", TypeSymbol.Int), New ParameterSymbol("string1", TypeSymbol.String), New ParameterSymbol("string2", TypeSymbol.String)), TypeSymbol.Int)
    Public ReadOnly LCase As New FunctionSymbol("LCASE$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.String)
    Public ReadOnly Len As New FunctionSymbol("LEN", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Int)
    Public ReadOnly MidFunction As New FunctionSymbol("MID$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("start", TypeSymbol.Int), New ParameterSymbol("num", TypeSymbol.int)), TypeSymbol.String)
    Public ReadOnly Right As New FunctionSymbol("RIGHT$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly Space As New FunctionSymbol("SPACE$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly Str As New FunctionSymbol("STR$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly StringFunction As New FunctionSymbol("STRING$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Int), New ParameterSymbol("value", TypeSymbol.Int)), TypeSymbol.String)
    Public ReadOnly UCase As New FunctionSymbol("UCASE$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.String)
    Public ReadOnly Val As New FunctionSymbol("VAL", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Int)
#End If
    Public ReadOnly Print As New FunctionSymbol("print", ImmutableArray.Create(New ParameterSymbol("text", TypeSymbol.String)), TypeSymbol.Nothing)
    Public ReadOnly Input As New FunctionSymbol("input", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.String)
    Public ReadOnly Rnd As New FunctionSymbol("rnd", ImmutableArray.Create(New ParameterSymbol("max", TypeSymbol.Integer)), TypeSymbol.Integer)

    Friend Function GetAll() As IEnumerable(Of FunctionSymbol)
      Return GetType(BuiltinFunctions).GetFields(BindingFlags.Public Or BindingFlags.Static).
                                       Where(Function(f) f.FieldType = GetType(FunctionSymbol)).
                                       Select(Function(f) CType(f.GetValue(Nothing), FunctionSymbol))
    End Function

  End Module

End Namespace
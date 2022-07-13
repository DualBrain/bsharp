Imports System.Collections.Immutable
Imports System.Reflection

Namespace Bsharp.CodeAnalysis.Symbols

  Friend Module BuiltinFunctions

    'Public ReadOnly Print As New FunctionSymbol("print", ImmutableArray.Create(New ParameterSymbol("text", TypeSymbol.String, 0)), TypeSymbol.Nothing)
    'Public ReadOnly Input As New FunctionSymbol("input", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.String)

    Public ReadOnly Asc As New FunctionSymbol("asc", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Long)
    Public ReadOnly Abs As New FunctionSymbol("abs", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Atn As New FunctionSymbol("atn", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Cos As New FunctionSymbol("cos", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Int As New FunctionSymbol("int", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly LBound As New FunctionSymbol("ubound", ImmutableArray.Create(New ParameterSymbol("lbound", TypeSymbol.Any)), TypeSymbol.Long)
    Public ReadOnly Len As New FunctionSymbol("len", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Long)
    Public ReadOnly Log As New FunctionSymbol("log", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Rnd As New FunctionSymbol("rnd", ImmutableArray.Create(New ParameterSymbol("max", TypeSymbol.Any, 0)), TypeSymbol.Long)
    Public ReadOnly Sgn As New FunctionSymbol("sgn", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Long)
    Public ReadOnly Sin As New FunctionSymbol("sin", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Sqr As New FunctionSymbol("sqr", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Tan As New FunctionSymbol("tan", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Double, 0)), TypeSymbol.Double)
    Public ReadOnly Val As New FunctionSymbol("val", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.Double)
    Public ReadOnly UBound As New FunctionSymbol("ubound", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Any)), TypeSymbol.Long)

    Public ReadOnly Chr As New FunctionSymbol("chr$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly Hex As New FunctionSymbol("hex$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly LCase As New FunctionSymbol("lcase$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.String)
    Public ReadOnly Left As New FunctionSymbol("left$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("num", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly Oct As New FunctionSymbol("oct$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly Right As New FunctionSymbol("right$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("num", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly Space As New FunctionSymbol("space$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly Str As New FunctionSymbol("str$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly UCase As New FunctionSymbol("ucase$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String)), TypeSymbol.String)

    Public ReadOnly Instr1 As New FunctionSymbol("instr", ImmutableArray.Create(New ParameterSymbol("string1", TypeSymbol.String), New ParameterSymbol("string2", TypeSymbol.String)), TypeSymbol.Long)
    Public ReadOnly Instr2 As New FunctionSymbol("instr", ImmutableArray.Create(New ParameterSymbol("start", TypeSymbol.Long), New ParameterSymbol("string1", TypeSymbol.String), New ParameterSymbol("string2", TypeSymbol.String)), TypeSymbol.Long)
    Public ReadOnly Mid1 As New FunctionSymbol("mid$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("start", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly Mid2 As New FunctionSymbol("mid$", ImmutableArray.Create(New ParameterSymbol("value", TypeSymbol.String), New ParameterSymbol("start", TypeSymbol.Long), New ParameterSymbol("length", TypeSymbol.Long)), TypeSymbol.String)
    Public ReadOnly StringFunction As New FunctionSymbol("string$", ImmutableArray.Create(New ParameterSymbol("num", TypeSymbol.Any), New ParameterSymbol("value", TypeSymbol.Any)), TypeSymbol.String)

    Friend Function GetAll() As IEnumerable(Of FunctionSymbol)
      Dim result = GetType(BuiltinFunctions).GetFields(BindingFlags.Public Or BindingFlags.Static).
                                             Where(Function(f) f.FieldType = GetType(FunctionSymbol)).
                                             Select(Function(f) CType(f.GetValue(Nothing), FunctionSymbol))
      Return result
    End Function

  End Module

End Namespace
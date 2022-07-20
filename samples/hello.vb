Option Explicit Off
Option Strict Off
Option Infer Off

Imports System

Namespace ConvertedFromBsharp

  Public Module Hello

    Public Sub Main()
      a = 10
      b = 3
      System.Console.Write("a = ")
      System.Console.Write(a)
      System.Console.Write(" : b = ")
      System.Console.Write(b)
      System.Console.Write(" : a \ b = ")
      System.Console.Write(a \ b)
      System.Console.WriteLine()
      Return
    End Sub

  End Module

End Namespace

Namespace Bsharp.CodeAnalysis.Emit

  Public Enum TargetPlatform
    Interpreter ' No "output", but will lower and prepare for use in the interpreter.
    MicrosoftVisualBasic ' Translate
    MicrosoftItermediateLanguage ' Bytecode / binary
    JavaByteCode ' Bytecode / binary
    Javascript ' Translate
    Typescript ' Strongly-typed Javascript
    WebAssembly ' Bytecode / binary
    C99 ' Need to decide which version of C/C++ will be supported.
    CC65 ' https://cc65.github.io
  End Enum

End Namespace
# B# (bsharp.net)

[![.NET Desktop](https://github.com/DualBrain/bsharp/actions/workflows/dotnet-desktop.yml/badge.svg)](https://github.com/DualBrain/bsharp/actions/workflows/dotnet-desktop.yml)

I've had an idea for several years now to create what most people would consider an absolutely absurd project - but it's something that just hasn't exited my brain.  Additionally, over the years, ideas have been either *in my head* or as simple separate experiments to test a few ideas. All of that changes with this project.

## Purpose?

The initial goal of the project is to recreate the *classic* BASIC experience but have this experience as a first-class citizen of .NET.  Once done, then move the project "forward" through time adding new features/capabilities without breaking this original *classic* experience.

Additionally, this project will strive to remain true to the original spirit of BASIC; as defined by the original creators of the language:

- (TODO - enter list here from a particular book after returning home.)

Finally, it has struck me as *odd* that given the power/capability of modern VB that such a project is written in a more evolved version of BASIC such as VB; the VB compiler, after all, is also written in VB... 

## Plans?

For now the rough list contains:

- Implement essentially a very GW-BASIC and/or QBasic like language on the latest version of .NET.
- This will include the necessary runtime to support the *classic* PC graphic, audio, etc. of GW-BASIC/QBasic.
- Add new language features that have come along in VB that can work within the context of GW-BASIC/QBasic.
- Will have a working interpreter to support the immediacy experience (REPL) that GW-BASIC/QBasic provided.
- Will compile to a .NET executable.
- Will contain a built in code-converter to VB (.NET) to allow for (if desired) "graduating" to a more modern BASIC.
- Considering targeting (code-to-code compiler) Javascript or WebAssembly.
- Will be built as a sort of "compiler-as-a-service" inspired by Roslyn - but will not be a clone; more just borrowing of ideas.
- Will have automated (unit) tests to help ensure stability over time.
- Evaluate (long-term) the idea of compiling to other "interesting" target platforms (Arduino, CoCo, C65, etc.)

## Status

The project is still very new and currently only has the lexer, parser, expression evaluator and a very few keywords implemented; in other words, not useful. However...

- All math operations implemented.
- Variable scope (for integer only) implemented.
- Rough implementations of `If`/`Then`, `For/Next`, `While`/`Wend`, and `Select Case` completed.

With that said, it's still not much more (visibly) than a "simple evaluator".

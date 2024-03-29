# B# (bsharp.net)

[![.NET Desktop](https://github.com/DualBrain/bsharp/actions/workflows/dotnet-desktop.yml/badge.svg)](https://github.com/DualBrain/bsharp/actions/workflows/dotnet-desktop.yml)

![B#](bsharp.png)

I've had an idea for several years now to create what most people would consider an absolutely absurd project - but it's something that just hasn't exited my brain.  Additionally, over the years, ideas have been either *in my head* or as simple separate experiments to test a few ideas. All of that changes with this project.

## Purpose

The initial goal of the project is to recreate the *classic* BASIC experience but have this experience as a first-class citizen of .NET.  Once done, then move the project "forward" through time adding new features/capabilities without breaking this original *classic* experience.

Additionally, this project will strive to adhere to the original principles of BASIC; as defined by the original creators of the language:

1. It should be easy to learn for the beginner.
2. It should be a general-purpose language, allowing the writing of any program.
3. Advanced features have to be added so that, if there was a price, it was paid by the expert, not the novice.
4. It should take full advantage of the fact that the user could interact with the computer.
5. It should give error messages that are clear and friendly to the user.
6. It should give fast response for small programs.
7. No understanding of the hardware should be necessary.
8. It should shield the user from the operating system.

As well as the above principles, I truly believe that BASIC would not have thrived without the work done by Microsoft; so this project will also be heavily influenced by the breadth of versions of Microsoft BASIC. Although the Dartmouth crowd had a negative view on what they referred to as "Street BASIC", I believe that the topic of BASIC today wouldn't even be an ongoing thriving topic if it weren't existence of these "Street BASIC"'s (namely the Microsoft variety).

Finally, it has struck me as *odd* that given the power/capability of modern VB that such a project is written in a more evolved version of BASIC such as VB; the VB compiler, after all, is also written in VB...

## Plan

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

I mention above GW-BASIC and/or QBasic, but should also include MBASIC as this is the precursor to GW-BASIC and where most of the current focus is at the moment since it's "less effort" to implement this language level related to cross-platform targetting.

For now the main goal is to get the language parsing and working within an interpreter.  Why an interpreter?  Immediate/instant results. Also, the interpreter is leveraging the same tool chain that the compiler(s) will eventually be utilizing; the only difference is that the interpreter will interpret and execute these in memory where the compiler(s) will emit code to disk.

A note regarding the compiler(s): some work on these has already begin with very early (prototype-level) work for .NET binaries, HTML/Javascript output and VB (.NET) code-converter done.  These are at a proof-of-concept stage to test aspects of the tool chain to ensure that I'm not painting myself into any corners as the project progresses.

## Status

See [Roadmap](https://github.com/DualBrain/bsharp/wiki/Roadmap).
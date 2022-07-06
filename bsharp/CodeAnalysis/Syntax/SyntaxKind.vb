Namespace Bsharp.CodeAnalysis.Syntax

  Public Enum SyntaxKind

    BadToken
    EndOfFileToken

    Label

    ' Trivia
    LineNumberTrivia
    SkippedTextTrivia
    LineBreakTrivia
    WhiteSpaceTrivia
    SingleLineCommentTrivia

    ' Tokens
    BackslashToken
    CloseBraceToken
    CloseParenToken
    ColonToken
    CommaToken
    EqualToken
    GreaterThanEqualToken
    GreaterThanToken
    HatToken
    IdentifierToken
    LessThanEqualToken
    LessThanGreaterThanToken
    LessThanToken
    MinusToken
    NumberToken
    OpenBraceToken
    OpenParenToken
    PeriodToken
    PlusToken
    PoundToken
    QuestionToken
    SemicolonToken
    SlashToken
    StarToken
    StringToken

    ' Keywords

    AndAlsoKeyword
    AndKeyword
    AsKeyword
    BaseKeyword
    ClearKeyword
    ClsKeyword
    ConstKeyword
    ContinueKeyword
    DimKeyword
    DoKeyword
    EachKeyword
    ElseIfKeyword
    ElseKeyword
    EndKeyword
    EndFunctionKeyword
    EndIfKeyword
    EqvKeyword
    ExitKeyword
    FalseKeyword
    ForKeyword
    FunctionKeyword
    GotoKeyword
    IfKeyword
    ImpKeyword
    InKeyword
    LetKeyword
    LoopKeyword
    MidKeyword
    ModKeyword
    NextKeyword
    NotKeyword
    OptionKeyword
    OrElseKeyword
    OrKeyword
    PrintKeyword
    ReturnKeyword
    SpcKeyword
    StepKeyword
    StopKeyword
    SystemKeyword
    TabKeyword
    ThenKeyword
    ToKeyword
    TrueKeyword
    UntilKeyword
    WendKeyword
    WhileKeyword
    XorKeyword

    ' Nodes
    CompilationUnit
    GlobalStatement
    FunctionDeclaration
    Parameter
    AsClause
    SingleLineElseClause
    WhileClause
    UntilClause

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
    ClearStatement
    ClsStatement
    ContinueStatement
    DoUntilStatement
    DoWhileStatement
    EndStatement
    ElseIfStatement
    ElseStatement
    ExitStatement
    ExpressionStatement
    ForEachStatement
    ForStatement
    GotoStatement
    IfStatement
    LabelStatement
    LetStatement
    MidStatement
    MultiLineIfBlock
    OptionStatement
    PrintStatement
    ReturnStatement
    SingleLineIfStatement
    StopStatement
    SystemStatement
    VariableDeclarationStatement
    WhileStatement

    ' Expressions
    LiteralExpression
    NameExpression
    UnaryExpression
    BinaryExpression
    ParenExpression
    AssignmentExpression
    CallExpression

  End Enum

End Namespace
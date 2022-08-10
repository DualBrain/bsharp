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
    AnyKeyword
    AsKeyword
    BaseKeyword
    ChDirKeyword
    ClearKeyword
    ClsKeyword
    ColorKeyword
    CommonKeyword
    ConstKeyword
    ContinueKeyword
    DataKeyword
    DefKeyword
    DefDblKeyword
    DefIntKeyword
    DefLngKeyword
    DefSngKeyword
    DefStrKeyword
    DimKeyword
    DoKeyword
    EachKeyword
    ElseIfKeyword
    ElseKeyword
    EndKeyword
    EndDefKeyword
    EndFunctionKeyword
    EndSubKeyword
    EndIfKeyword
    EndTypeKeyword
    EqvKeyword
    EraseKeyword
    ErrorKeyword
    ExitKeyword
    FalseKeyword
    ForKeyword
    FunctionKeyword
    GosubKeyword
    GotoKeyword
    IfKeyword
    ImpKeyword
    InKeyword
    InputKeyword
    KillKeyword
    LetKeyword
    'LineInputKeyword
    LocateKeyword
    LockKeyword
    LoopKeyword
    MidKeyword
    MkDirKeyword
    ModKeyword
    NameKeyword
    NextKeyword
    NotKeyword
    OptionKeyword
    OrElseKeyword
    OrKeyword
    PrintKeyword
    RandomKeyword
    RandomizeKeyword
    ReadKeyword
    RedimKeyword
    RemKeyword
    ResetKeyword
    RestoreKeyword
    ReturnKeyword
    RmDirKeyword
    SharedKeyword
    SpcKeyword
    SleepKeyword
    StaticKeyword
    StepKeyword
    StopKeyword
    SubKeyword
    SwapKeyword
    SystemKeyword
    TabKeyword
    ThenKeyword
    ToKeyword
    TrueKeyword
    TypeKeyword
    UnlockKeyword
    UntilKeyword
    WendKeyword
    WhileKeyword
    WidthKeyword
    WindowKeyword
    WriteKeyword
    XorKeyword

    ' Nodes
    AsClause
    CompilationUnit
    DefDeclaration
    FunctionDeclaration
    GlobalStatement
    Parameter
    SingleLineElseClause
    UntilClause
    WhileClause

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
    ChDirStatement
    ClearStatement
    ClsStatement
    ContinueStatement
    DataStatement
    DoUntilStatement
    DoWhileStatement
    EndStatement
    ElseIfStatement
    ElseStatement
    ExitStatement
    ExpressionStatement
    ForEachStatement
    ForStatement
    GosubStatement
    GotoStatement
    IfStatement
    InputStatement
    KillStatement
    LabelStatement
    LetStatement
    MidStatement
    MkDirStatement
    MultiLineIfBlock
    NameStatement
    OptionStatement
    PrintStatement
    ReadStatement
    RemStatement
    RestoreStatement
    ReturnGosubStatement
    ReturnStatement
    RmDirStatement
    SingleLineDefDeclaration
    SingleLineIfStatement
    StopStatement
    SystemStatement
    VariableDeclarationStatement
    WhileStatement

    ' Expressions
    AssignmentExpression
    BinaryExpression
    CallExpression
    LiteralExpression
    NameExpression
    ParenExpression
    UnaryExpression

  End Enum

End Namespace
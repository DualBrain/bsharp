Namespace Basic.CodeAnalysis.Syntax

  Public Enum SyntaxKind

    BadToken

    ' Trivia
    SkippedTextTrivia
    LineBreakTrivia
    WhiteSpaceTrivia
    SingleLineCommentTrivia
    MultiLineCommentTrivia

    ' Tokens
    EndOfFileToken
    WhitespaceToken
    NumberToken
    PlusToken
    MinusToken
    StarToken
    SlashToken
    BackslashToken
    HatToken
    OpenParenToken
    CloseParenToken
    EqualToken
    LessThanToken
    PeriodToken
    ColonToken
    CommaToken
    SemicolonToken
    QuestionToken
    GreaterThanEqualToken
    LessThanEqualToken
    LessThanGreaterThanToken
    GreaterThanToken
    OpenBraceToken
    CloseBraceToken
    IdentifierToken
    StringToken
    PipeToken
    DollarToken

    ' Keywords
    FalseKeyword
    TrueKeyword
    ModKeyword
    NotKeyword
    AndKeyword
    AndAlsoKeyword
    OrKeyword
    OrElseKeyword
    XorKeyword
    EqvKeyword
    ImpKeyword

    ConstKeyword
    DimKeyword
    LetKeyword

    DoKeyword

    ' Potential start of a Command 
    EndKeyword

    ' Blocks (NOTE: Commands are a combination of 2 or more keywords that make up a command "phrase".)
    FunctionKeyword
    IfKeyword
    ThenKeyword
    ElseIfKeyword
    ElseKeyword
    WhileKeyword
    WendKeyword
    ForKeyword
    EachKeyword
    InKeyword
    ToKeyword
    StepKeyword
    NextKeyword
    'SelectKeyword
    'CaseKeyword
    'IsKeyword
    AsKeyword
    LoopKeyword
    UntilKeyword
    ContinueKeyword
    ExitKeyword
    ReturnKeyword

    ' Nodes
    CompilationUnit
    GlobalStatement
    FunctionDeclaration
    Parameter
    AsClause
    SingleLineElseClause
    'CaseElseClauseSyntax
    'CaseClauseSyntax
    WhileClause
    UntilClause

    ' Statements
    BlockStatement
    VariableDeclaration
    ExpressionStatement
    SingleLineIfStatement
    MultiLineIfBlock
    IfStatement
    ElseIfStatement
    ElseStatement
    'SelectCaseStatement
    'CaseMatchExpressionSyntax
    ForEachStatement
    ForStatement
    WhileStatement
    DoWhileStatement
    DoUntilStatement
    ExitStatement
    ContinueStatement
    ReturnStatement

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
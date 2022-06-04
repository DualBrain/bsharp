Namespace Basic.CodeAnalysis.Syntax

  Public Enum SyntaxKind

    ' Tokens
    BadToken
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
    IfKeyword
    ThenKeyword
    ElseIfKeyword
    ElseKeyword
    WhileKeyword
    WendKeyword
    ForKeyword
    ToKeyword
    StepKeyword
    NextKeyword
    SelectKeyword
    CaseKeyword
    IsKeyword

    ' Nodes
    CompilationUnit

    ' Statements
    BlockStatement
    VariableDeclaration
    ExpressionStatement
    IfStatement
    ElseClause
    WhileStatement
    ForStatement
    ElseIfClause
    CaseMatchExpressionSyntax
    CaseElseClauseSyntax
    CaseClauseSyntax
    SelectCaseStatement

    ' Expressions
    LiteralExpression
    NameExpression
    UnaryExpression
    BinaryExpression
    ParenthesizedExpression
    AssignmentExpression
    CallExpression
    DoWhileStatement

    'FunctionKeyword
    'VarKeyword
    'Parameter
    'FunctionDeclaration
    'TypeClause

  End Enum

End Namespace
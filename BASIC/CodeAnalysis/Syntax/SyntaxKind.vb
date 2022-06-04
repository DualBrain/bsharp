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

    ' Expressions
    LiteralExpression
    NameExpression
    UnaryExpression
    BinaryExpression
    ParenthesizedExpression
    AssignmentExpression
    ElseIfClause
    SelectKeyword
    CaseKeyword
    IsKeyword
    CaseMatchExpressionSyntax
    CaseElseClauseSyntax
    CaseClauseSyntax
    SelectCaseStatement
    'FunctionKeyword
    'VarKeyword
    'Parameter
    'FunctionDeclaration
    'TypeClause

  End Enum

End Namespace
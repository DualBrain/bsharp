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
    FunctionKeyword
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
    AsKeyword

    ' Nodes
    CompilationUnit
    FunctionDeclaration
    GlobalStatement
    Parameter
    TypeClause

    ' Statements
    BlockStatement
    VariableDeclaration
    ExpressionStatement
    IfStatement
    ElseStatement
    WhileStatement
    ForStatement
    ElseIfStatement
    CaseMatchExpressionSyntax
    CaseElseClauseSyntax
    CaseClauseSyntax
    SelectCaseStatement
    DoWhileStatement

    ' Expressions
    LiteralExpression
    NameExpression
    UnaryExpression
    BinaryExpression
    ParenthesizedExpression
    AssignmentExpression
    CallExpression
    SingleLineElseClause
    SingleLineIfStatement
    MultiLineIfBlock
  End Enum

End Namespace
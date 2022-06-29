Namespace Bsharp.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
    ClearStatement
    ClsStatement
    ConditionalGotoStatement
    DoUntilStatement
    DoWhileStatement
    EndStatement
    ElseIfStatement
    ExpressionStatement
    ForStatement
    GotoStatement
    IfStatement
    LabelStatement
    LetStatement
    NopStatement
    OptionStatement
    PrintStatement
    ReturnStatement
    StopStatement
    SystemStatement
    VariableDeclaration
    WhileStatement

    ' Expressions
    BinaryExpression
    AssignmentExpression
    CallExpression
    ConversionExpression
    ErrorExpression
    LiteralExpression
    VariableExpression
    UnaryExpression

  End Enum

End Namespace
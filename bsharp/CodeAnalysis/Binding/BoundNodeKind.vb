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
    MidStatement
    NopStatement
    OptionStatement
    PrintStatement
    RemStatement
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
    HandlePrintLineStatement
    HandlePrintStatement
    HandleTabStatement
    HandleSpcStatement
    HandleCommaStatement

  End Enum

End Namespace
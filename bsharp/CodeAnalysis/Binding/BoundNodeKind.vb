Namespace Bsharp.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
    ChDirStatement
    ClearStatement
    ClsStatement
    ConditionalGotoStatement
    DoUntilStatement
    DoWhileStatement
    EndStatement
    ElseIfStatement
    ExpressionStatement
    ForStatement
    GosubStatement
    GotoStatement
    HandleCommaStatement
    HandlePrintLineStatement
    HandlePrintStatement
    HandleTabStatement
    HandleSpcStatement
    IfStatement
    InputStatement
    KillStatement
    LabelStatement
    LetStatement
    MidStatement
    MkDirStatement
    NameStatement
    NopStatement
    OptionStatement
    PrintStatement
    RemStatement
    ReturnGosubStatement
    ReturnStatement
    RmDirStatement
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
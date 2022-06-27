Namespace Basic.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
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
    PrintStatement
    ReturnStatement
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
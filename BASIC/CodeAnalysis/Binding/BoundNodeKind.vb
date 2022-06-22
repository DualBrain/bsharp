Namespace Basic.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

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
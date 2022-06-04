Namespace Basic.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    ' Statements
    NopStatement
    BlockStatement
    VariableDeclaration
    ExpressionStatement
    IfStatement
    ElseIfStatement
    WhileStatement
    ForStatement
    DoWhileStatement

    SelectCaseStatement
    BoundMatchStatement
    BoundCaseStatement

    GotoStatement
    LabelStatement
    ConditionalGotoStatement

    ' Expressions
    ErrorExpression
    LiteralExpression
    VariableExpression
    AssignmentExpression
    UnaryExpression
    BinaryExpression
    CallExpression
    ConversionExpression

  End Enum

End Namespace
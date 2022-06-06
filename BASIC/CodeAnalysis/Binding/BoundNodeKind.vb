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
    DoUntilStatement

    'SelectCaseStatement
    'BoundMatchStatement
    'BoundCaseStatement

    GotoStatement
    LabelStatement
    ConditionalGotoStatement
    ReturnStatement

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
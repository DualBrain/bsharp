Namespace Basic.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    ' Statements
    BlockStatement
    VariableDeclaration
    ExpressionStatement
    IfStatement
    ElseIfStatement
    WhileStatement
    ForStatement

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
    NopStatement
  End Enum

End Namespace
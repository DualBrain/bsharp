Namespace Basic.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    ' Statements
    BlockStatement
    VariableDeclaration
    ExpressionStatement
    IfStatement
    WhileStatement
    ForStatement

    ' Expressions
    LiteralExpression
    VariableExpression
    AssignmentExpression
    UnaryExpression
    BinaryExpression

  End Enum

End Namespace
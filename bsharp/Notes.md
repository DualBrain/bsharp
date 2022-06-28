

General steps to add something:

- Start with the SyntaxFacts.
  - Need a new SyntaxKind.
  - Handle this SyntaxKind appropriately in GetKeywordKind and GetText.
- If it is a "statement"...
  - Add entry to ParseStatement
  - Create new method "Parse___Statement"
    - Handle "popping" tokens as needed.
- Now need to handle Binding, see Binder.
  - Create an entry in BoundNodeKind.
  - If a statement, BindStatement...
    - Create "Bind____Statement" method.
      - Handle the Binding (from Syntax); will will most likely mean creating a new Bound___Expression class.
- Now we need to update the Evaluator.
  - EvaluateStatement...
    - Evaluate___Statement
      - 
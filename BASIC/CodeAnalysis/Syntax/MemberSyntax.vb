﻿Namespace Basic.CodeAnalysis.Syntax

  Public MustInherit Class MemberSyntax
    Inherits SyntaxNode

    Friend Sub New(tree As SyntaxTree)
      MyBase.New(tree)
    End Sub

  End Class

End Namespace
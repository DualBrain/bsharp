Imports System.IO
Imports Bsharp.CodeAnalysis.Symbols
Imports Bsharp.CodeAnalysis.Syntax
Imports System.CodeDom.Compiler

Namespace Bsharp.CodeAnalysis.Binding

  Friend NotInheritable Class ControlFlowGraph

    Private Sub New(start As BasicBlock, [end] As BasicBlock, blocks As List(Of BasicBlock), branches As List(Of BasicBlockBranch))
      Me.Start = start
      Me.End = [end]
      Me.Blocks = blocks
      Me.Branches = branches
    End Sub

    Public ReadOnly Property Start() As BasicBlock
    Public ReadOnly Property [End]() As BasicBlock
    Public ReadOnly Property Blocks() As List(Of BasicBlock)
    Public ReadOnly Property Branches() As List(Of BasicBlockBranch)

    Public NotInheritable Class BasicBlock

      Public Sub New()
      End Sub

      Public Sub New(isStart As Boolean)
        Me.IsStart = isStart
        IsEnd = Not isStart
      End Sub

      Public ReadOnly Property IsStart() As Boolean
      Public ReadOnly Property IsEnd() As Boolean
      Public ReadOnly Property Statements() As New List(Of BoundStatement)()
      Public ReadOnly Property Incoming() As New List(Of BasicBlockBranch)()
      Public ReadOnly Property Outgoing() As New List(Of BasicBlockBranch)()

      Public Overrides Function ToString() As String

        If IsStart Then
          Return "<Start>"
        End If

        If IsEnd Then
          Return "<End>"
        End If

        Using writer = New StringWriter()
          Using indentedWriter = New IndentedTextWriter(writer)
            For Each statement In Statements
              statement.WriteTo(indentedWriter)
            Next
            Return writer.ToString()
          End Using
        End Using

      End Function

    End Class

    Public NotInheritable Class BasicBlockBranch

      Public Sub New(from As BasicBlock, [to] As BasicBlock, condition As BoundExpression)
        Me.From = from
        Me.To = [to]
        Me.Condition = condition
      End Sub

      Public ReadOnly Property From() As BasicBlock
      Public ReadOnly Property [To]() As BasicBlock
      Public ReadOnly Property Condition() As BoundExpression

      Public Overrides Function ToString() As String
        If Condition Is Nothing Then
          Return String.Empty
        End If
        Return Condition.ToString()
      End Function

    End Class

    Public NotInheritable Class BasicBlockBuilder

      Private ReadOnly m_statements As New List(Of BoundStatement)()
      Private ReadOnly m_blocks As New List(Of BasicBlock)()

      Public Function Build(block As BoundBlockStatement) As List(Of BasicBlock)
        For Each statement In block.Statements
          Select Case statement.Kind
            Case BoundNodeKind.LabelStatement
              StartBlock()
              m_statements.Add(statement)
            Case BoundNodeKind.ConditionalGotoStatement,
                 BoundNodeKind.GotoStatement,
                 BoundNodeKind.ReturnStatement
              m_statements.Add(statement)
              StartBlock()
            Case BoundNodeKind.ClearStatement,
                 BoundNodeKind.ClsStatement,
                 BoundNodeKind.EndStatement,
                 BoundNodeKind.ExpressionStatement,
                 BoundNodeKind.HandleCommaStatement,
                 BoundNodeKind.HandlePrintLineStatement,
                 BoundNodeKind.HandlePrintStatement,
                 BoundNodeKind.HandleSpcStatement,
                 BoundNodeKind.HandleTabStatement,
                 BoundNodeKind.LetStatement,
                 BoundNodeKind.NopStatement,
                 BoundNodeKind.OptionStatement,
                 BoundNodeKind.PrintStatement,
                 BoundNodeKind.StopStatement,
                 BoundNodeKind.SystemStatement,
                 BoundNodeKind.VariableDeclaration
              m_statements.Add(statement)
            Case Else
              Throw New Exception($"Unexpected statement: {statement.Kind}")
          End Select
        Next
        EndBlock()
        Return m_blocks.ToList()
      End Function

      Private Sub StartBlock()
        EndBlock()
      End Sub

      Private Sub EndBlock()
        If m_statements.Count > 0 Then
          Dim block = New BasicBlock()
          block.Statements.AddRange(m_statements)
          m_blocks.Add(block)
          m_statements.Clear()
        End If
      End Sub

    End Class

    Public NotInheritable Class GraphBuilder

      Private ReadOnly m_blockFromStatement As New Dictionary(Of BoundStatement, BasicBlock)()
      Private ReadOnly m_blockFromLabel As New Dictionary(Of BoundLabel, BasicBlock)()
      Private ReadOnly m_branches As New List(Of BasicBlockBranch)()
      Private ReadOnly m_start As New BasicBlock(isStart:=True)
      Private ReadOnly m_end As New BasicBlock(isStart:=False)

      Public Function Build(blocks As List(Of BasicBlock)) As ControlFlowGraph
        If Not blocks.Any() Then
          Connect(m_start, m_end)
        Else
          Connect(m_start, blocks.First())
        End If

        For Each block In blocks
          For Each statement In block.Statements
            m_blockFromStatement.Add(statement, block)
            If TypeOf statement Is BoundLabelStatement Then
              Dim labelStatement = CType(statement, BoundLabelStatement)
              m_blockFromLabel.Add(labelStatement.Label, block)
            End If
          Next
        Next

        For i = 0 To blocks.Count - 1
          Dim current = blocks(i)
          Dim [next] = If(i = blocks.Count - 1, m_end, blocks(i + 1))
          For Each statement In current.Statements
            Dim isLastStatementInBlock = statement Is current.Statements.Last()
            Select Case statement.Kind
              Case BoundNodeKind.GotoStatement
                Dim gs = CType(statement, BoundGotoStatement)

                Dim toBlock As BasicBlock = Nothing
                If m_blockFromLabel.Keys.Contains(gs.Label) Then
                  toBlock = m_blockFromLabel(gs.Label)
                Else
                  For Each entry In m_blockFromLabel.Keys
                    If entry.Name = gs.Label.Name Then
                      toBlock = m_blockFromLabel(entry)
                      Exit For
                    End If
                  Next
                End If
                'Dim toBlock = m_blockFromLabel(gs.Label)
                Connect(current, toBlock)
              Case BoundNodeKind.ConditionalGotoStatement
                Dim cgs = CType(statement, BoundConditionalGotoStatement)
                Dim thenBlock = m_blockFromLabel(cgs.Label)
                Dim elseBlock = [next]
                Dim negatedCondition = Negate(cgs.Condition)
                Dim thenCondition = If(cgs.JumpIfTrue, cgs.Condition, negatedCondition)
                Dim elseCondition = If(cgs.JumpIfTrue, negatedCondition, cgs.Condition)
                Connect(current, thenBlock, thenCondition)
                Connect(current, elseBlock, elseCondition)
              Case BoundNodeKind.ReturnStatement
                Connect(current, m_end)
              Case BoundNodeKind.ClearStatement,
                   BoundNodeKind.ClsStatement,
                   BoundNodeKind.EndStatement,
                   BoundNodeKind.ExpressionStatement,
                   BoundNodeKind.HandleCommaStatement,
                   BoundNodeKind.HandlePrintLineStatement,
                   BoundNodeKind.HandlePrintStatement,
                   BoundNodeKind.HandleSpcStatement,
                   BoundNodeKind.HandleTabStatement,
                   BoundNodeKind.LabelStatement,
                   BoundNodeKind.LetStatement,
                   BoundNodeKind.NopStatement,
                   BoundNodeKind.OptionStatement,
                   BoundNodeKind.PrintStatement,
                   BoundNodeKind.StopStatement,
                   BoundNodeKind.SystemStatement,
                   BoundNodeKind.VariableDeclaration
                If isLastStatementInBlock Then
                  Connect(current, [next])
                End If
              Case Else
                Throw New Exception($"Unexpected statement: {statement.Kind}")
            End Select
          Next
        Next

ScanAgain:
        For Each block In blocks
          If Not block.Incoming.Any() Then
            RemoveBlock(blocks, block)
            GoTo ScanAgain
          End If
        Next block

        blocks.Insert(0, m_start)
        blocks.Add(m_end)

        Return New ControlFlowGraph(m_start, m_end, blocks, m_branches)

      End Function

      Private Sub Connect(from As BasicBlock, [to] As BasicBlock, Optional condition As BoundExpression = Nothing)

        If TypeOf condition Is BoundLiteralExpression Then
          Dim l = CType(condition, BoundLiteralExpression)
          Dim value = CBool(l.Value)
          If value Then
            condition = Nothing
          Else
            Return
          End If
        End If

        Dim branch = New BasicBlockBranch(from, [to], condition)
        from.Outgoing.Add(branch)
        [to].Incoming.Add(branch)
        m_branches.Add(branch)

      End Sub

      Private Sub RemoveBlock(blocks As List(Of BasicBlock), block As BasicBlock)

        For Each branch In block.Incoming
          branch.From.Outgoing.Remove(branch)
          m_branches.Remove(branch)
        Next

        For Each branch In block.Outgoing
          branch.To.Incoming.Remove(branch)
          m_branches.Remove(branch)
        Next

        blocks.Remove(block)

      End Sub

      Private Function Negate(condition As BoundExpression) As BoundExpression
        If TypeOf condition Is BoundLiteralExpression Then
          Dim literal = CType(condition, BoundLiteralExpression)
          Dim value = CBool(literal.Value)
          Return New BoundLiteralExpression(Not value)
        End If
        Dim op = BoundUnaryOperator.Bind(SyntaxKind.NotKeyword, TypeSymbol.Boolean)
        Debug.Assert(op IsNot Nothing)
        Return New BoundUnaryExpression(op, condition)
      End Function
    End Class

    Private Function Quote(text As String) As String
      Return """" & text.Replace("""", "\""") & """"
      Return """" & text.TrimEnd().Replace("\", "\\").Replace("""", "\""").Replace(Environment.NewLine, "\l") & """"
    End Function

    Public Sub WriteTo(writer As TextWriter)

      writer.WriteLine("digraph G {")

      Dim blockIds = New Dictionary(Of BasicBlock, String)()

      For i = 0 To Blocks.Count - 1
        Dim id = $"N{i}"
        blockIds.Add(Blocks(i), id)
      Next

      For Each block In Blocks
        Dim id = blockIds(block)
        Dim label = Quote(block.ToString()) '.Replace(Environment.NewLine, "\l"))
        writer.WriteLine($"    {id} [label = {label} shape = box]")
      Next

      For Each branch In Branches
        Dim fromId = blockIds(branch.From)
        Dim toId = blockIds(branch.To)
        Dim label = Quote(branch.ToString())
        writer.WriteLine($"    {fromId} -> {toId} [label = {label}]")
      Next

      writer.WriteLine("}")

    End Sub

    Public Shared Function Create(body As BoundBlockStatement) As ControlFlowGraph
      Dim basicBlockBuilder = New BasicBlockBuilder()
      Dim blocks = basicBlockBuilder.Build(body)
      Dim graphBuilder = New GraphBuilder()
      Return graphBuilder.Build(blocks)
    End Function

    Public Shared Function AllPathsReturn(body As BoundBlockStatement) As Boolean

      Dim graph = Create(body)

      For Each branch In graph.End.Incoming
        'Dim lastStatement = branch.From.Statements.Last()
        'If lastStatement.Kind <> BoundNodeKind.ReturnStatement Then
        Dim lastStatement = branch.From.Statements.LastOrDefault
        If lastStatement Is Nothing OrElse lastStatement.Kind <> BoundNodeKind.ReturnStatement Then
          Return False
        End If
      Next

      Return True

    End Function

  End Class

End Namespace
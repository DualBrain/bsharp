Imports Bsharp.CodeAnalysis.Symbols

Namespace Bsharp.CodeAnalysis.Binding

  Friend Module ConstantFolding

    Public Function ComputeConstant(op As BoundUnaryOperator, operand As BoundExpression) As BoundConstant
      If operand.ConstantValue IsNot Nothing AndAlso TypeOf operand.ConstantValue.Value Is Integer Then
        Select Case op.Kind
          Case BoundUnaryOperatorKind.Identity
            Return New BoundConstant(CInt(operand.ConstantValue.Value))
          Case BoundUnaryOperatorKind.Negation
            Return New BoundConstant(-CInt(operand.ConstantValue.Value))
          Case BoundUnaryOperatorKind.LogicalNegation
            Return New BoundConstant(Not CBool(operand.ConstantValue.Value))
          Case BoundUnaryOperatorKind.BitwiseComplement
            Return New BoundConstant(Not CInt(operand.ConstantValue.Value))
          Case Else
            Throw New Exception($"Unexpected unary operator {op.Kind}")
        End Select
      End If
      Return Nothing
    End Function

    Public Function Fold(left As BoundExpression, op As BoundBinaryOperator, right As BoundExpression) As BoundConstant

      Dim leftConstant = left.ConstantValue
      Dim rightConstant = right.ConstantValue

      ' Special case `And` and `Or` because there are cases where only need one side needs to be known.

      If op.Kind = BoundBinaryOperatorKind.LogicalAnd Then
        If (leftConstant IsNot Nothing AndAlso Not CBool(leftConstant.Value)) OrElse
           (rightConstant IsNot Nothing AndAlso Not CBool(rightConstant.Value)) Then
          Return New BoundConstant(False)
        End If
      End If

      If op.Kind = BoundBinaryOperatorKind.LogicalOr Then
        If (leftConstant IsNot Nothing AndAlso CBool(leftConstant.Value)) OrElse
           (rightConstant IsNot Nothing AndAlso CBool(rightConstant.Value)) Then
          Return New BoundConstant(True)
        End If
      End If

      If leftConstant Is Nothing OrElse rightConstant Is Nothing Then Return Nothing

      ' compute

      Dim l = leftConstant.Value
      Dim r = rightConstant.Value

      Select Case op.Kind
        Case BoundBinaryOperatorKind.Raise
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) ^ CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) ^ CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) ^ CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) ^ CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) ^ CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) ^ CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) ^ CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) ^ CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) ^ CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) ^ CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) ^ CByte(r))
          End Select
        Case BoundBinaryOperatorKind.Addition
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) + CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) + CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) + CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) + CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) + CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) + CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) + CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) + CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) + CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) + CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) + CByte(r))
            Case TypeSymbol.Type.String : Return New BoundConstant(CStr(l) & CStr(r))
          End Select
        Case BoundBinaryOperatorKind.Subtraction : Return New BoundConstant(CInt(l) - CInt(r))
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) - CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) - CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) - CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) - CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) - CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) - CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) - CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) - CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) - CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) - CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) - CByte(r))
          End Select
        Case BoundBinaryOperatorKind.Multiplication
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) * CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) * CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) * CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) * CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) * CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) * CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) * CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) * CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) * CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) * CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) * CByte(r))
          End Select
        Case BoundBinaryOperatorKind.Division
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) / CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) / CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) / CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) / CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) / CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) / CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) / CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) / CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) / CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) / CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) / CByte(r))
          End Select
        Case BoundBinaryOperatorKind.BitwiseAnd
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) And CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) And CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) And CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) And CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) And CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) And CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) And CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) And CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) And CBool(r))
          End Select
        Case BoundBinaryOperatorKind.BitwiseOr
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) Or CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) Or CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) Or CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) Or CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) Or CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) Or CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) Or CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) Or CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) Or CBool(r))
          End Select
        Case BoundBinaryOperatorKind.BitwiseXor
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) Xor CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) Xor CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) Xor CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) Xor CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) Xor CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) Xor CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) Xor CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) Xor CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) Xor CBool(r))
          End Select
        Case BoundBinaryOperatorKind.LogicalAnd
          Return New BoundConstant(CBool(l) And CBool(r))
        Case BoundBinaryOperatorKind.LogicalOr
          Return New BoundConstant(CBool(l) Or CBool(r))
        Case BoundBinaryOperatorKind.LogicalXor
          Return New BoundConstant(CBool(l) Xor CBool(r))
        Case BoundBinaryOperatorKind.LogicalOrElse
          Return New BoundConstant(CBool(l) OrElse CBool(r))
        Case BoundBinaryOperatorKind.LogicalAndAlso
          Return New BoundConstant(CBool(l) AndAlso CBool(r))
        Case BoundBinaryOperatorKind.Equal
          Return New BoundConstant(Equals(l, r))
        Case BoundBinaryOperatorKind.NotEqual
          Return New BoundConstant(Not Equals(l, r))
        Case BoundBinaryOperatorKind.LessThan
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) < CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) < CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) < CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) < CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) < CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) < CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) < CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) < CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) < CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) < CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) < CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) < CBool(r))
            Case TypeSymbol.Type.String : Return New BoundConstant(CStr(l) < CStr(r))
          End Select
        Case BoundBinaryOperatorKind.GreaterThan
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) > CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) > CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) > CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) > CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) > CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) > CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) > CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) > CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) > CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) > CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) > CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) > CBool(r))
            Case TypeSymbol.Type.String : Return New BoundConstant(CStr(l) > CStr(r))
          End Select
        Case BoundBinaryOperatorKind.LessThanEqual
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) <= CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) <= CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) <= CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) <= CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) <= CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) <= CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) <= CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) <= CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) <= CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) <= CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) <= CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) <= CBool(r))
            Case TypeSymbol.Type.String : Return New BoundConstant(CStr(l) <= CStr(r))
          End Select
        Case BoundBinaryOperatorKind.GreaterThanEqual
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) >= CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) >= CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) >= CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) >= CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) >= CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) >= CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) >= CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) >= CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) >= CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) >= CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) >= CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) >= CBool(r))
            Case TypeSymbol.Type.String : Return New BoundConstant(CStr(l) >= CStr(r))
            Case Else
          End Select
        Case Else
          Throw New Exception($"Unexpected binary operator {op.Kind}")
      End Select

      Throw New Exception($"Invalid binary operator for {TypeSymbol.TypeSymbolToType(left.Type)} {op.Kind} {TypeSymbol.TypeSymbolToType(right.Type)}")

    End Function

  End Module

End Namespace
// FOR NEXT TIME: custom types, functions
object Typechecker {
  def typecheckProgram(program: Program): Unit = {
    program.stmts.foldLeft(Map[Variable, Type]())((currentTypeEnv, currentStmt) =>
      typecheckStmt(currentStmt, currentTypeEnv))
  }

  def typecheckStmt(stmt: Stmt, typeEnv: Map[Variable, Type]): Map[Variable, Type] = {
    stmt match {
      case VardecStmt(typ, userVar, init) => {
        if (typeEnv.contains(userVar)) {
          throw new IllTypedException(s"Variable already in scope: $userVar")
        }
        val expType = typeof(init, typeEnv)
        if (typ != expType) {
          throw new IllTypedException(s"Expected type: $typ; got type: $expType");
        }
        typeEnv + (userVar -> typ)
      }
    }
  }

  def typeof(exp: Exp, typeEnv: Map[Variable, Type]): Type = {
    exp match {
      case VariableExp(userVar) => {
        if (typeEnv.contains(userVar)) {
          typeEnv.apply(userVar)
        } else {
          throw new IllTypedException(s"Not in scope: $userVar")
        }
      }
      case IntegerLiteralExp(_) => IntType
      case BooleanLiteralExp(_) => BoolType
      case BinopExp(left, op, right) => {
        (typeof(left, typeEnv), op, typeof(right, typeEnv)) match {
          case (IntType, PlusOp, IntType) => IntType
          case (BoolType, AndOp, BoolType) => BoolType
          case (IntType, LessThanOp, IntType) => BoolType
          case (leftType, EqualsOp, rightType) if leftType == rightType => BoolType
          case _ => throw new IllTypedException(s"leftType: $leftType; rightType: $rightType; op: $op; is ill-typed")
        }
      }
    }
  } // typeof
}


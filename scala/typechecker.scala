// FOR NEXT TIME: custom types, functions
object Typechecker {
  type StructInfo = Map[StructName, Map[StructField, Type]]

  def makeStructInfo(program: Program): StructInfo = {
    val structInfo: StructInfo = program.structDecs.map(structDec => {
      val innerMap = structDef.fields.map(field => (field._2 -> field._1)).toMap
      // ensure that we don't have duplicate field names
      if (innerMap.size != structDef.fields.size) {
        throw new IllTypedException("Duplicate field name on struct: " + structDec.structName)
      }
      (structDec.structName -> innerMap)
    }).toMap

    // ensure that we don't have duplicate struct names
    if (structInfo.size != program.structDecs.size) {
      throw new IllTypedException("Duplicate struct name")
    }

    // ensure all the types on the fields are ok
    structInfo.values.foreach(_.values.foreach(typ => ensureTypeOk(typ, structInfo)))

    structInfo
  }

  def ensureTypeOk(typ: Type, structInfo: StructInfo): Unit = {
    typ match {
      case IntType | BoolType => ()
      case StructNameType(structName) if structInfo.contains(structName) => ()
      case _ => throw new IllTypedException("Unrecognized type: " + typ)
    }
  }

  def typecheckProgram(program: Program): Unit = {
    val structInfo = makeStructInfo(program)

    program.stmts.foldLeft(Map[Variable, Type]())((currentTypeEnv, currentStmt) =>
      typecheckStmt(currentStmt, currentTypeEnv))
  }

  def typecheckStmt(
    stmt: Stmt,
    typeEnv: Map[Variable, Type],
    structInfo: StructInfo): Map[Variable, Type] = {
    stmt match {
      case VardecStmt(typ, userVar, init) => {
        ensureTypeOk(typ, structInfo)
        if (typeEnv.contains(userVar)) {
          throw new IllTypedException(s"Variable already in scope: $userVar")
        }
        val expType = typeof(init, typeEnv, structInfo)
        if (typ != expType) {
          throw new IllTypedException(s"Expected type: $typ; got type: $expType");
        }
        typeEnv + (userVar -> typ)
      }
    }
  }

  def typeof(
    exp: Exp,
    typeEnv: Map[Variable, Type],
    structInfo: StructInfo): Type = {
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
        (typeof(left, typeEnv, structInfo), op, typeof(right, typeEnv, structInfo)) match {
          case (IntType, PlusOp, IntType) => IntType
          case (BoolType, AndOp, BoolType) => BoolType
          case (IntType, LessThanOp, IntType) => BoolType
          case (leftType, EqualsOp, rightType) if leftType == rightType => BoolType
          case _ => throw new IllTypedException(s"leftType: $leftType; rightType: $rightType; op: $op; is ill-typed")
        }
      }
    }
    case DotExp(exp, fieldName) => {
      val expType = typeof(exp, typeEnv, structInfo)
      expType match {
        case StructNameType(structName) => {
          if (structInfo.contains(structName) && structInfo(structName).contains(fieldName)) {
            structInfo(structName)(fieldName)
          } else {
            throw new IllTypedException("Bad struct access")
          }
        }
        case _ => throw new IllTypedException("Attempt to access field of non-struct")
      }
    }
    case NewExp(structName, fields) => {
      val fieldsMap: Map[StructField, Exp] = fields.toMap
      if (fields.size != fieldsMap.size) {
        throw new IllTypedException("Duplicate field name provided for new")
      }
      if (!structInfo.contains(structName)) {
        throw new IllTypedException("No structure declared: " + structName)
      }
      val infoForThisStruct: Map[StructField, Type] = structInfo(structName)
      if (fieldsMap.keySet != infoForThisStruct.keySet) {
        throw new IllTypedException("Naming mismatch on fields")
      }
      if (fieldsMap.view.mapValues(exp =>
        typeof(exp, typeEnv, structInfo)) != infoForThisStruct) {
        throw new IllTypedException("One or more field names have incorrect types")
      }
      StructNameType(structName)
    }
  } // typeof
}


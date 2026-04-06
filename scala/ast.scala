case class Variable(name: String)
case class StructName(name: String)
case class StructField(name: String)

// structdec ::= `struct` STRUCT_NAME `{` (type STRUCT_FIELD `;`)* `}`
case class StructDec(structName: StructName, fields: Seq[(Type, StructField)])

// exp ::= VARIABLE | INTEGER | `true` | `false` | exp op exp
//         exp `.` STRUCT_FIELD | `new` STRUCT_NAME `{` fielddecs `}`
sealed trait Exp
case class VariableExp(name: Variable) extends Exp
case class IntegerLiteralExp(value: Int) extends Exp
case class BooleanLiteralExp(value: Boolean) extends Exp
case class BinopExp(left: Exp, op: Op, right: Exp) extends Exp
case class DotExp(exp: Exp, structField: StructField) extends Exp
case class NewExp(structName: StructName, fielddecs: Seq[(StructField, Exp)]) extends Exp

// op ::= `+` | `&&` | `<` | `==`
sealed trait Op
case object PlusOp extends Op
case object AndOp extends Op
case object LessThanOp extends Op
case object EqualsOp extends Op

// stmt ::= type VARIABLE `=` exp `;`
sealed trait Stmt
case class VardecStmt(typ: Type, variable: Variable, initializer: Exp) extends Stmt

case class Program(structDecs: Seq[StructDec], stmts: Seq[Stmt])

// type ::= `int` | `bool` | STRUCT_NAME
sealed trait Type
case object IntType extends Type
case object BoolType extends Type
case class StructNameType(structName: StructName) extends Type


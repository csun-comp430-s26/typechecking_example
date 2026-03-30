case class Variable(name: String)

// exp ::= VARIABLE | INTEGER | `true` | `false` | exp op exp
sealed trait Exp
case class VariableExp(name: Variable) extends Exp
case class IntegerLiteralExp(value: Int) extends Exp
case class BooleanLiteralExp(value: Boolean) extends Exp
case class BinopExp(left: Exp, op: Op, right: Exp) extends Exp

// op ::= `+` | `&&` | `<` | `==`
sealed trait Op
case object PlusOp extends Op
case object AndOp extends Op
case object LessThanOp extends Op
case object EqualsOp extends Op

// stmt ::= type VARIABLE `=` exp `;`
sealed trait Stmt
case class VardecStmt(typ: Type, variable: Variable, initializer: Exp) extends Stmt

case class Program(stmts: Seq[Stmt])

// type ::= `int` | `bool`
sealed trait Type
case object IntType extends Type
case object BoolType extends Type



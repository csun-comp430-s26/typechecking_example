public class Typechecker {
    // def typeof(exp: Exp): Type = {
    //   exp match {
    //     case IntegerLiteralExp(_) => IntType
    //     case BooleanLiteralExp(_) => BoolType
    //     case BinopExp(leftExp, op, rightExp) => {
    //       (typeof(leftExp), op, typeof(rightExp)) match {
    //         case (IntType, PlusOp, IntType) => IntType
    //         case (BoolType, AndOp, BoolType) => BoolType
    //         case (IntType, LessThanOp, IntType) => BoolType
    //         case (t1, EqualsOp, t2) if t1 == t2 => BoolType
    //       }
    //     }
    //   }
    // }
    public static Type typeof(final Exp exp) throws IllTypedException {
        if (exp instanceof IntegerLiteralExp) {
            // 42: int
            return new IntType();
        } else if (exp instanceof BooleanLiteralExp) {
            // true: bool
            return new BoolType();
        } else if (exp instanceof BinopExp binopExp) {
            // exp op exp: ???
            final Type leftType = typeof(binopExp.left());
            final Type rightType = typeof(binopExp.right());
            if (leftType instanceof IntType &&
                rightType instanceof IntType &&
                binopExp.op() instanceof PlusOp) {
                // int + int = int
                return new IntType();
            } else if (leftType instanceof BoolType &&
                       rightType instanceof BoolType &&
                       binopExp.op() instanceof AndOp) {
                // bool && bool = bool
                return new BoolType();
            } else if (leftType instanceof IntType &&
                       rightType instanceof IntType &&
                       binopExp.op() instanceof LessThanOp) {
                // int < int = bool
                return new BoolType();
            } else if (binopExp.op() instanceof EqualsOp &&
                       leftType.equals(rightType)) {
                // T == T = bool
                //    int == int = bool
                //    bool == bool = bool
                return new BoolType();
            } else {
                throw new IllTypedException("leftType: " + leftType +
                                            "rightType: " + rightType +
                                            "op: " + binopExp.op() +
                                            " is ill typed");
            }
        } else {
            throw new IllTypedException("Unrecognized expression: " + exp);
        }
    }
}

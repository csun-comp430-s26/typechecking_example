import java.util.Map;
import java.util.HashMap;

public class Typechecker {
    // FOR NEXT TIME: port to Scala, structures
    public static void typecheckProgram(final Program program) throws IllTypedException {
        final Map<Variable, Type> typeEnv = new HashMap<Variable, Type>();
        for (final Stmt stmt : program.stmts()) {
            typecheckStmt(stmt, typeEnv);
        }
    }
    
    public static void typecheckStmt(final Stmt stmt,
                                     final Map<Variable, Type> typeEnv) throws IllTypedException {


        if (stmt instanceof VardecStmt vardecStmt) {
            if (typeEnv.containsKey(vardecStmt.variable())) {
                throw new IllTypedException("Variable already in scope: " + vardecStmt.variable());
            }
            final Type expType = typeof(vardecStmt.initializer(), typeEnv);
            if (!vardecStmt.type().equals(expType)) {
                throw new IllTypedException("Expected type: " + vardecStmt.type() +
                                            "; got type: " + expType);
            }
            typeEnv.put(vardecStmt.variable(), vardecStmt.type());
        } else {
            throw new IllTypedException("Unrecognized statement: " + stmt);
        }
    }
    
    public static Type typeof(final Exp exp,
                              final Map<Variable, Type> typeEnv) throws IllTypedException {
        if (exp instanceof VariableExp varExp) {
            if (typeEnv.containsKey(varExp.name())) {
                return typeEnv.get(varExp.name());
            } else {
                throw new IllTypedException("Not in scope: " + varExp.name());
            }
        } else if (exp instanceof IntegerLiteralExp) {
            // 42: int
            return new IntType();
        } else if (exp instanceof BooleanLiteralExp) {
            // true: bool
            return new BoolType();
        } else if (exp instanceof BinopExp binopExp) {
            // exp op exp: ???
            final Type leftType = typeof(binopExp.left(), typeEnv);
            final Type rightType = typeof(binopExp.right(), typeEnv);
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

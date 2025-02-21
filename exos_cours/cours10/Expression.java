import java.util.Optional;

public abstract class Expression {

    abstract Value evaluate(Environment env);
}

class BooleanExpression extends Expression {
    boolean value;

    @Override
    Value evaluate(Environment env) {
        return new BooleanValue(value);
    }
}

class NumberExpression extends Expression {
    int value;

    @Override
    Value evaluate(Environment env) {
        return new NumberValue(value);
    }
}

class IdentifierExpression extends Expression {
    String name;

    @Override
    Value evaluate(Environment env) {
        Optional<Value> inter = env.lookup(name);
        if (inter.isPresent()){
            return inter.get();
        }else{
            throw new RuntimeException("Undeclared " + name);
        }
    }
}

class UnaryExpression extends Expression {
    UnaryOp operator;
    Expression expression;

    @Override
    Value evaluate(Environment env) {
        Value eval = expression.evaluate(env);
        switch (operator) {
            case NOT:
                return new BooleanValue(!eval.asBoolean());
            case MINUS:
                return new NumberValue(-eval.asInt());
            default:
                throw new RuntimeException("Unsupported operator " + operator);
        }
    }
}

enum UnaryOp {
    MINUS,
    NOT
}

class BinaryExpression extends Expression {
    Expression left;
    Expression right;
    BinaryOp operator;

    @Override
    Value evaluate(Environment env) {
        int lhs = left.evaluate(env).asInt();
        int rhs = right.evaluate(env).asInt();

        switch (operator){
            case SUB:
                return new NumberValue(lhs - rhs);
            case ADD:
                return new NumberValue(lhs + rhs);
            case TIMES:
                return new NumberValue(lhs * rhs);
            default:
                throw new RuntimeException("Unsupported operator " + operator);
        }
    }
}

enum BinaryOp {
    SUB,
    ADD,
    TIMES
}

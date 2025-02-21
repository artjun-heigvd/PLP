import java.util.Optional;

public abstract class Statement {
    abstract void execute(Environment env);
}

class AssignmentStatement extends Statement {
    IdentifierExpression identifier;
    Expression expression;

    @Override
    void execute(Environment env) {
        env.upset(identifier.name, expression.evaluate(env));        
    }
}

class SequenceStatement extends Statement {
    Statement first;
    Statement second;

    @Override
    void execute(Environment env) {
        first.execute(env);
        second.execute(env);
    }
}

class IfStatement extends Statement {
    Expression condition;
    Statement then;
    Optional<Statement> elze;

    @Override
    void execute(Environment env) {
        boolean evalCond = condition.evaluate(env).asBoolean();
        if (evalCond){
            then.execute(env);
            return;
        }

        elze.ifPresent(els -> els.execute(env));
    }
}

class WhileStatement extends Statement {
    Expression condition;
    Statement body;
 
    @Override
    void execute(Environment env) {
        while (condition.evaluate(env).asBoolean()){
            body.execute(env);
        }
    }
}
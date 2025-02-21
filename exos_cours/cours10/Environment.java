import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

class Environment {
    Map<String, Value> bindings = new HashMap<>();
    
    void upset(String name, Value value){
        bindings.put(name, value);
    }

    Optional<Value> lookup(String name){
        return Optional.ofNullable(bindings.get(name));
    }
}

public abstract class Value {
    boolean asBoolean(){
        if (this instanceof BooleanValue){
            return ((BooleanValue) this).value;
        }else{
            throw new RuntimeException("Is not a boolean");
        }
    }

    int asInt(){
        if (this instanceof NumberValue){
            return ((NumberValue) this).value;
        }else{
            throw new RuntimeException("Is not a number");
        }
    }
}

class BooleanValue extends Value {
    boolean value;

    BooleanValue(boolean value){
        this.value = value;
    }
}

class NumberValue extends Value {
    int value;

    NumberValue(int value){
        this.value = value;
    }
}
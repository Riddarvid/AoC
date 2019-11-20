package AoC.day7.wires;

public class Set extends OneInputWire {
    public Set(String id, String o1) {
        super(id, o1);
    }

    @Override
    int getValueSpecific() {
        return v1;
    }

    @Override
    public String toString() {
        return o1;
    }
}

package AoC.day7.wires;

public class And extends TwoInputWire {
    public And(String id, String o1, String o2) {
        super(id, o1, o2);
    }

    @Override
    int getValueSpecific() {
        return v1 & v2;
    }

    @Override
    public String toString() {
        return o1 + " AND " + o2;
    }
}

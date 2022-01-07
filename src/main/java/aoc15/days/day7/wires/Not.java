package aoc15.days.day7.wires;

public class Not extends OneInputWire {

    public Not(String id, String o1) {
        super(id, o1);
    }

    @Override
    int getValueSpecific() {
        return (65535 - v1);
    }

    @Override
    public String toString() {
        return "NOT " + o1;
    }
}

package aoc15.days.day7.wires;

public class Or extends TwoInputWire {
    public Or(String id, String o1, String o2) {
        super(id, o1, o2);
    }

    @Override
    int getValueSpecific() {
        return v1 | v2;
    }

    @Override
    public String toString() {
        return o1 + " OR " + o2;
    }
}

package AoC.day7.wires;

public abstract class TwoInputWire extends Wire {
    protected String o1;
    protected String o2;
    protected int v1;
    protected int v2;

    public TwoInputWire(String id, String o1, String o2) {
        super(id);
        this.o1 = o1;
        this.o2 = o2;
    }

    @Override
    void calculateInputs() {
        if (Character.isDigit(o1.charAt(0))) {
            v1 = Integer.parseInt(o1);
        } else {
            v1 = wires.get(o1).getValue();
        }
        if (Character.isDigit(o2.charAt(0))) {
            v2 = Integer.parseInt(o2);
        } else {
            v2 = wires.get(o2).getValue();
        }
    }
}

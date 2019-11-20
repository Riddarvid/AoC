package AoC.day7.wires;

public abstract class OneInputWire extends Wire {
    protected String o1;
    protected int v1;

    public OneInputWire(String id, String o1) {
        super(id);
        this.o1 = o1;
    }

    @Override
    void calculateInputs() {
        if (Character.isDigit(o1.charAt(0))) {
            v1 = Integer.parseInt(o1);
        } else {
            v1 = wires.get(o1).getValue();
        }
    }
}

package AoC.Day161921;

public class Instruction {
    private int opCode;
    private final int a;
    private final int b;
    private final int output;

    public Instruction(int opCode, int a, int b, int output) {
        this.opCode = opCode;
        this.a = a;
        this.b = b;
        this.output = output;
    }

    public Instruction(int a, int b, int output) {
        this.a = a;
        this.b = b;
        this.output = output;
    }

    public int getOpCode() {
        return opCode;
    }

    public int getA() {
        return a;
    }

    public int getB() {
        return b;
    }

    public int getOutput() {
        return output;
    }
}

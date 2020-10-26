package aoc.days.Day161921;

import aoc.days.Day161921.OpCodes.OpcodeFunction;

public class KnownInstruction {
    public static int instructionPointerRegister;

    private final OpcodeFunction operation;
    private final int a;
    private final int b;
    private final int output;

    public KnownInstruction(OpcodeFunction operation, int a, int b, int output) {
        this.operation = operation;
        this.a = a;
        this.b = b;
        this.output = output;
    }

    public OpcodeFunction getOperation() {
        return operation;
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

    public Memory apply(Memory memory) {
        Setup setup = new Setup(memory, new Instruction(a, b, output));
        return operation.apply(setup);
    }

    @Override
    public String toString() {
        return operation.toString();
    }
}

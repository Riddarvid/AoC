package AoC.Day161921;

public class Setup {
    private Memory memory;
    private Instruction instruction;

    public Setup(Memory memory, Instruction instruction) {
        this.memory = memory;
        this.instruction = instruction;
    }

    public Memory getMemory() {
        return memory;
    }

    public Instruction getInstruction() {
        return instruction;
    }
}

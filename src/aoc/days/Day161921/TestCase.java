package aoc.days.Day161921;

public class TestCase {
    Memory expected;
    Setup setup;

    public TestCase(Memory before, Memory expected, Instruction instruction) {
        this.setup = new Setup(before, instruction);
        this.expected = expected;
    }
}

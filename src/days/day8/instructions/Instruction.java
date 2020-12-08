package days.day8.instructions;

import days.day8.Console;

public interface Instruction {
    void apply(Console console);

    String getName();

    int getValue();
}

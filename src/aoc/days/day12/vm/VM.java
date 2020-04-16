package aoc.days.day12.vm;

import java.util.*;

public class VM {
    final Map<Character, Integer> registers;
    final PC pc;

    public VM(Collection<Character> registerNames) {
        pc = new PC();
        registers = new HashMap<>();
        for (char c : registerNames) {
            registers.put(c, 0);
        }
    }

    public void execute(List<Instruction> program) {
        while (pc.getPosition() >= 0 && pc.getPosition() < program.size()) {
            program.get(pc.getPosition()).execute(registers, pc);
        }
    }

    public int getRegister(char register) {
        if (!registers.containsKey(register)) {
            throw new InputMismatchException("No such register");
        }
        return registers.get(register);
    }

    public void setRegister(char register, int value) {
        registers.put(register, value);
    }
}

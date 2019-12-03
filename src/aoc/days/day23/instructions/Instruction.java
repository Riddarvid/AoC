package aoc.days.day23;

public abstract class Instruction {
    long valueOf(String input, VM vm) {
        if (Character.isDigit(input.charAt(0)) || input.charAt(0) == '-') {
            return Long.parseLong(input);
        }
        return vm.get(input.charAt(0));
    }

    public abstract void execute(VM vm);
}

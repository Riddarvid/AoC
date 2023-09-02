package aoc.days.day21;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day21 extends Day {
    private List<Instruction> instructions;

    public static void main(String[] args) {
        new Day21();
    }

    @Override
    protected void part1() {
        Password password = new Password("abcdefgh");
        password.scramble(instructions);
        System.out.println(password.toString());
    }

    @Override
    protected void part2() {
        //Alternativ lösning: Testa alla permutationer
        Password password = new Password("fbgdceah");
        password.unscramble(instructions);
        System.out.println(password.toString());
    }

    @Override
    protected void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = InputUtils.getTokens(s, " ");
            List<String> parameters = new ArrayList<>();
            switch (tokens.get(0)) {
                case "swap":
                    parameters.add(tokens.get(2));
                    parameters.add(tokens.get(5));
                    if (tokens.get(1).equals("position")) {
                        instructions.add(new Instruction(Operation.SWAP_POSITION, parameters));
                    } else {
                        instructions.add(new Instruction(Operation.SWAP_LETTER, parameters));
                    }
                    break;
                case "rotate":
                    if (tokens.get(1).equals("based")) {
                        parameters.add(tokens.get(6));
                        instructions.add(new Instruction(Operation.ROTATE_LETTER, parameters));
                    } else {
                        parameters.add(tokens.get(1));
                        parameters.add(tokens.get(2));
                        instructions.add(new Instruction(Operation.ROTATE_STEPS, parameters));
                    }
                    break;
                case "reverse":
                    parameters.add(tokens.get(2));
                    parameters.add(tokens.get(4));
                    instructions.add(new Instruction(Operation.REVERSE, parameters));
                    break;
                case "move":
                    parameters.add(tokens.get(2));
                    parameters.add(tokens.get(5));
                    instructions.add(new Instruction(Operation.MOVE, parameters));
                    break;
                default:
                    throw new InputMismatchException(tokens.get(0));
            }
        }
    }
}

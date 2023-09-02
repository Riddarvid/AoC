package aoc.days.day25;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.*;

public class Day25 extends Day {
    List<Instruction> program;

    public static void main(String[] args) {
        new Day25();
    }

    @Override
    protected void part1() {
        VM vm = new VM(getCharRange('a', 'd'));
        int a = 0;
        vm.setRegister('a', a);
        while (!vm.execute(program)) {
            //System.out.println(a);
            a++;
            vm = new VM(getCharRange('a', 'd'));
            vm.setRegister('a', a);
        }
        System.out.println(a);
    }

    @Override
    protected void part2() {

    }

    private Collection<Character> getCharRange(char first, char last) {
        Collection<Character> characters = new HashSet<>();
        for (char c = first; c <= last; c++) {
            characters.add(c);
        }
        return characters;
    }

    @Override
    protected void setup() {
        program = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = InputUtils.getTokens(s, " ");
            List<String> arguments = new ArrayList<>();
            for (int i = 1; i < tokens.size(); i++) {
                arguments.add(tokens.get(i));
            }
            Operation operation;
            switch (tokens.get(0)) {
                case "cpy":
                    operation = Operation.CPY;
                    break;
                case "inc":
                    operation = Operation.INC;
                    break;
                case "dec":
                    operation = Operation.DEC;
                    break;
                case "jnz":
                    operation = Operation.JNZ;
                    break;
                case "tgl":
                    operation = Operation.TGL;
                    break;
                case "out":
                    operation = Operation.OUT;
                    break;
                default:
                    throw new InputMismatchException("Illegal instruction " + tokens.get(0));
            }
            program.add(new Instruction(operation, arguments));
        }
    }
}

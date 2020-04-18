package aoc.days.day23;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.*;

public class Day23 extends Day {
    private List<Instruction> program1;
    private List<Instruction> program2;

    public static void main(String[] args) {
        new Day23();
    }

    @Override
    protected void part1() {
        VM vm = new VM(getCharRange('a', 'd'));
        vm.setRegister('a', 7);
        vm.execute(program1);
        System.out.println(vm.getRegister('a'));
    }

    @Override
    protected void part2() {
        /*VM vm = new VM(getCharRange('a', 'd'));
        vm.setRegister('a', 12);
        vm.execute(program2);
        System.out.println(vm.getRegister('a'));*/
        /*int a = 0;
        int b = 0;
        int c = 0;
        int d = 0;

        a = 12;
        b = 12;

        do {
            b--;
            d = a;
            a = a * b;

            b--;
            c = b;
            d = c;

        }*/
        int a = 12;
        for (int b = 11; b > 0; b--) {
            a *= b;
        }
        a += 99 * 94;
        System.out.println(a);
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
        program1 = new ArrayList<>();
        program2 = new ArrayList<>();
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
                default:
                    throw new InputMismatchException("Illegal instruction " + tokens.get(0));
            }
            program1.add(new Instruction(operation, arguments));
            program2.add(new Instruction(operation, arguments));
        }
    }
}

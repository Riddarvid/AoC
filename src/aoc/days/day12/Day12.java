package aoc.days.day12;

import aoc.days.Day;
import aoc.days.day12.vm.*;
import aoc.utils.input.InputUtils;

import java.util.*;

public class Day12 extends Day {
    private List<Instruction> program;

    public static void main(String[] args) {
        new Day12();
    }

    @Override
    protected void part1() {
        VM vm = new VM(getCharRange('a','d'));
        vm.execute(program);
        System.out.println(vm.getRegister('a'));
    }

    private Collection<Character> getCharRange(char first, char last) {
        Collection<Character> characters = new HashSet<>();
        for (char c = first; c <= last; c++) {
            characters.add(c);
        }
        return characters;
    }

    @Override
    protected void part2() {
        VM vm = new VM(getCharRange('a','d'));
        vm.setRegister('c', 1);
        vm.execute(program);
        System.out.println(vm.getRegister('a'));
    }

    @Override
    protected void setup() {
        program = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = InputUtils.getTokens(s, ' ');
            switch (tokens.get(0)) {
                case "cpy":
                    if (Character.isLetter(tokens.get(1).charAt(0))) {
                        program.add(new CpyRegister(tokens.get(1).charAt(0), tokens.get(2).charAt(0)));
                    } else {
                        program.add(new CpyValue(Integer.parseInt(tokens.get(1)), tokens.get(2).charAt(0)));
                    }
                    break;
                case "inc":
                    program.add(new Inc(tokens.get(1).charAt(0)));
                    break;
                case "dec":
                    program.add(new Dec(tokens.get(1).charAt(0)));
                    break;
                case "jnz":
                    if (Character.isLetter(tokens.get(1).charAt(0))) {
                        program.add(new JnzRegister(tokens.get(1).charAt(0), Integer.parseInt(tokens.get(2))));
                    } else {
                        program.add(new JnzValue(Integer.parseInt(tokens.get(1)), Integer.parseInt(tokens.get(2))));
                    }
                    break;
                default:
                    throw new InputMismatchException("Illegal instruction " + tokens.get(0));
            }
        }
    }
}

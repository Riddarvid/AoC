package aoc.days.day18;

import aoc.days.Day;
import aoc.days.day18.instructions.*;
import aoc.days.day18.instructions.Set;
import aoc.utils.InputUtilities;

import java.util.*;

public class Day18 extends Day {
    private VM vm;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        vm.run();
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        Map<Character, Long> registers = new HashMap<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            for (int i = 1; i < tokens.size(); i++) {
                if (Character.isAlphabetic(tokens.get(i).charAt(0))) {
                    registers.put(tokens.get(i).charAt(0), 0L);
                }
            }
        }
        List<Instruction> instructions = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            switch (tokens.get(0)) {
                case "snd":
                    instructions.add(new Snd(tokens.get(1)));
                    break;
                case "set":
                    instructions.add(new Set(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "add":
                    instructions.add(new Add(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "mul":
                    instructions.add(new Mul(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "mod":
                    instructions.add(new Mod(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "rcv":
                    instructions.add(new Rcv(tokens.get(1).charAt(0)));
                    break;
                case "jgz":
                    instructions.add(new Jgz(tokens.get(1), tokens.get(2)));
                    break;
                default:
                    throw new InputMismatchException("Invalid instruction");
            }
        }
        vm = new VM(registers, instructions);
    }
}

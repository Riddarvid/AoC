package aoc.days.day18;

import aoc.days.Day;
import aoc.days.day18.instructions.*;
import aoc.days.day18.instructions.Set;
import aoc.utils.InputUtilities;

import java.util.*;

public class Day18 extends Day {
    private Executer executer0;
    private Executer executer1;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        Thread a = new Thread(executer0);
        Thread b = new Thread(executer1);
        a.start();
        b.start();
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        Map<Character, Long> registers0 = new HashMap<>();
        Map<Character, Long> registers1 = new HashMap<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            for (int i = 1; i < tokens.size(); i++) {
                if (Character.isAlphabetic(tokens.get(i).charAt(0))) {
                    registers0.put(tokens.get(i).charAt(0), 0L);
                    registers1.put(tokens.get(i).charAt(0), 0L);
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
        VM vm0 = new VM(registers0, 0);
        vm0.put('p', 0);
        VM vm1 = new VM(registers1, 1);
        vm1.put('p', 1);
        vm0.setOther(vm1);
        vm1.setOther(vm0);
        executer0 = new Executer(vm0, instructions);
        executer1 = new Executer(vm1, instructions);
    }
}

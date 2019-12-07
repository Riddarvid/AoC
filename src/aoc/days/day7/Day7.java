package aoc.days.day7;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.ArrayList;
import java.util.List;

public class Day7 extends Day {
    private int[] program;

    public static void main(String[] args) {
        new Day7();
    }

    @Override
    protected void part1() {
        List<List<Integer>> settings = generateSettingsRecursive(new ArrayList<>());
        int maxOut = 0;
        for (List<Integer> setting : settings) {
            List<Amplifier> amplifiers = new ArrayList<>();
            for (int i = 0; i < 5; i++) {
                amplifiers.add(new Amplifier(Character.toString('A' + i), setting.get(i), program));
            }
            for (int i = 0; i < 4; i++) {
                amplifiers.get(i).setNext(amplifiers.get(i + 1));
            }
            amplifiers.get(0).addInput(0);
            List<Thread> threads = new ArrayList<>();
            for (int i = 0; i < 4; i++) {
                threads.add(new Thread(amplifiers.get(i)));
                threads.get(i).start();
            }
            amplifiers.get(4).run();
            if (amplifiers.get(4).getOutput() > maxOut) {
                maxOut = amplifiers.get(4).getOutput();
            }
        }
        System.out.println(maxOut);
    }

    private List<List<Integer>> generateSettingsRecursive(List<Integer> soFar) {
        if (soFar.size() == 5) {
            List<List<Integer>> output = new ArrayList<>();
            output.add(soFar);
            return output;
        } else {
            List<List<Integer>> output = new ArrayList<>();
            for (int i = 0; i <= 4; i++) {
                if (!soFar.contains(i)) {
                    List<Integer> in = new ArrayList<>(soFar);
                    in.add(i);
                    output.addAll(generateSettingsRecursive(in));
                }
            }
            return output;
        }
    }

    @Override
    protected void part2() {
        List<List<Integer>> settings = generateSettingsRecursive(new ArrayList<>());
        int maxOut = 0;
        for (List<Integer> setting : settings) {
            int lastOutput = run(setting);
            if (lastOutput > maxOut) {
                maxOut = lastOutput;
            }
        }
        System.out.println(maxOut);
    }

    private int run(List<Integer> setting) {
        List<Amplifier> amplifiers = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            amplifiers.add(new Amplifier(Character.toString('A' + i), setting.get(i) + 5, program));
        }
        for (int i = 0; i < 4; i++) {
            amplifiers.get(i).setNext(amplifiers.get(i + 1));
        }
        amplifiers.get(4).setNext(amplifiers.get(0));
        amplifiers.get(0).addInput(0);
        List<Thread> threads = new ArrayList<>();
        for (int i = 0; i < 4; i++) {
            threads.add(new Thread(amplifiers.get(i)));
            threads.get(i).start();
        }
        amplifiers.get(4).run();
        return amplifiers.get(4).getOutput();
    }

    @Override
    protected void setup() {
        String s = lines.get(0);
        List<Integer> values = InputUtilities.getIntsNegative(s);
        program = new int[values.size()];
        for (int i = 0; i < program.length; i++) {
            program[i] = values.get(i);
        }
    }
}

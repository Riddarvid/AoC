package aoc.days.day7;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.math.MathUtils;

import java.util.ArrayList;
import java.util.List;

public class Day7 extends Day {
    private int[] program;

    public static void main(String[] args) {
        new Day7();
    }

    @Override
    protected void part1() {
        int nAmplifiers = 5;
        List<Integer> validSettings = new ArrayList<>();
        validSettings.add(0);
        validSettings.add(1);
        validSettings.add(2);
        validSettings.add(3);
        validSettings.add(4);
        List<List<Integer>> settings = MathUtils.generatePermutations(validSettings);
        int maxOut = 0;
        for (List<Integer> setting : settings) {
            List<Amplifier> amplifiers = generateAmplifiers(nAmplifiers, setting);
            connectAmplifiers(amplifiers, false);
            amplifiers.get(0).addInput(0);
            createAndStartThreads(amplifiers, false);
            amplifiers.get(4).run();
            if (amplifiers.get(4).getOutput() > maxOut) {
                maxOut = amplifiers.get(4).getOutput();
            }
        }
        System.out.println(maxOut);
    }

    private void createAndStartThreads(List<Amplifier> amplifiers, boolean startLast) {
        for (int i = 0; i < amplifiers.size() - 1; i++) {
            new Thread(amplifiers.get(i)).start();
        }
        if (startLast) {
            new Thread(amplifiers.get(amplifiers.size() - 1)).start();
        }
    }

    private List<Amplifier> generateAmplifiers(int n, List<Integer> phaseSetting) {
        List<Amplifier> amplifiers = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            amplifiers.add(new Amplifier(phaseSetting.get(i), program));
        }
        return amplifiers;
    }

    private void connectAmplifiers(List<Amplifier> amplifiers, boolean feedBack) {
        for (int i = 0; i < amplifiers.size() - 1; i++) {
            amplifiers.get(i).setNext(amplifiers.get(i + 1));
        }
        if (feedBack) {
            amplifiers.get(amplifiers.size() - 1).setNext(amplifiers.get(0));
        }
    }

    @Override
    protected void part2() {
        List<Integer> validSettings = new ArrayList<>();
        validSettings.add(5);
        validSettings.add(6);
        validSettings.add(7);
        validSettings.add(8);
        validSettings.add(9);
        List<List<Integer>> settings = MathUtils.generatePermutations(validSettings);
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
        int nAmplifiers = 5;
        List<Amplifier> amplifiers = generateAmplifiers(nAmplifiers, setting);
        connectAmplifiers(amplifiers, true);
        amplifiers.get(0).addInput(0);
        createAndStartThreads(amplifiers, false);
        amplifiers.get(4).run();
        return amplifiers.get(4).getOutput();
    }

    @Override
    protected void setup() {
        String s = lines.get(0);
        List<Integer> values = InputUtils.getIntsNegative(s);
        program = new int[values.size()];
        for (int i = 0; i < program.length; i++) {
            program[i] = values.get(i);
        }
    }
}

package aoc.days.day7;

import aoc.utils.Controller;
import aoc.utils.IntcodeComputer;

import java.util.Arrays;
import java.util.LinkedList;

public class Amplifier implements Controller, Runnable {
    private String id;
    private int phaseSetting;
    private LinkedList<Integer> inputs;
    private boolean hasSetPhase;
    private int output;
    private int[] program;
    private Amplifier next;

    public Amplifier(String id, int phaseSetting, int[] program) {
        this.id = id;
        this.phaseSetting = phaseSetting;
        inputs = new LinkedList<>();
        this.program = Arrays.copyOf(program, program.length);
        hasSetPhase = false;
    }

    public void setNext(Amplifier next) {
        this.next = next;
    }

    @Override
    public int getInput() {
        if (hasSetPhase) {
            synchronized (this) {
                while (inputs.isEmpty()) {
                    try {
                        wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
                int input = inputs.getFirst();
                inputs.removeFirst();
                return input;
            }
        } else {
            hasSetPhase = true;
            return phaseSetting;
        }
    }

    public void addInput(int input) {
        synchronized (this) {
            inputs.addLast(input);
            notifyAll();
        }
    }

    @Override
    public void output(int val) {
        output = val;
        if (next != null) {
            next.addInput(output);
        }
    }

    public int getOutput() {
        return output;
    }

    public void run() {
        new IntcodeComputer(this).execute(program);
    }

    @Override
    public String toString() {
        return id + " " +phaseSetting + " -> " + next.id;
    }
}

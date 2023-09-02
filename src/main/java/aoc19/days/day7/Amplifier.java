package aoc19.days.day7;

import aoc19.utils.intcode.Controller;
import aoc19.utils.intcode.IntcodeComputer;

import java.util.Arrays;
import java.util.LinkedList;

public class Amplifier implements Controller, Runnable {
    private LinkedList<Long> inputs;
    private long output;
    private long[] program;
    private Amplifier next;

    public Amplifier(long phaseSetting, long[] program) {
        inputs = new LinkedList<>();
        inputs.addLast(phaseSetting);
        this.program = Arrays.copyOf(program, program.length);
    }

    public void setNext(Amplifier next) {
        this.next = next;
    }

    @Override
    public long getInput() {
        synchronized (this) {
            while (inputs.isEmpty()) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            long input = inputs.getFirst();
            inputs.removeFirst();
            return input;
        }
    }

    public void addInput(long input) {
        synchronized (this) {
            inputs.addLast(input);
            notifyAll();
        }
    }

    @Override
    public void output(long val) {
        output = val;
        if (next != null) {
            next.addInput(output);
        }
    }

    public long getOutput() {
        return output;
    }

    public void run() {
        new IntcodeComputer(this, program).execute();
    }
}

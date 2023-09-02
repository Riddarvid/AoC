package aoc19.days.day15;

import aoc19.utils.intcode.Controller;
import aoc19.utils.intcode.IntcodeComputer;

public class Communicator implements Runnable, Controller {
    private long[] memory;
    private boolean hasRequest;
    private int request;
    private boolean hasOutput;
    private int output;

    public Communicator(long[] memory) {
        this.memory = memory;
    }

    @Override
    public void run() {
        new IntcodeComputer(this, memory).execute();
    }

    @Override
    public long getInput() {
        synchronized (this) {
            while (!hasRequest) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            hasRequest = false;
            return request;
        }
    }

    @Override
    public void output(long val) {
        synchronized (this) {
            if (hasOutput) {
                throw new IllegalStateException("Last output never requested");
            }
            hasOutput = true;
            output = (int)val;
            notifyAll();
        }
    }

    public void makeRequest(int request) {
        synchronized (this) {
            if (hasRequest) {
                throw new IllegalStateException("Last output never requested");
            }
            hasRequest = true;
            this.request = request;
            notifyAll();
        }
    }

    public int getStatus() {
        synchronized (this) {
            while (!hasOutput) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            hasOutput = false;
            return output;
        }
    }
}

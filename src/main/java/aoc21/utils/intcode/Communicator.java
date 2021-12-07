package aoc21.utils.intcode;

import java.util.LinkedList;

public abstract class Communicator implements Runnable, Controller {
    private long[] memory;
    protected LinkedList<Integer> requests;
    protected LinkedList<Integer> outputs;
    private boolean running;

    public Communicator(long[] memory) {
        this.memory = memory;
        requests = new LinkedList<>();
        outputs = new LinkedList<>();
    }

    @Override
    public void run() {
        synchronized (this) {
            running = true;
        }
        new IntcodeComputer(this, memory).execute();
        synchronized (this) {
            running = false;
        }
    }

    @Override
    public long getInput() {
        synchronized (this) {
            while (requests.isEmpty()) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            int request = requests.getFirst();
            requests.removeFirst();
            return request;
        }
    }

    @Override
    public abstract void output(long val);

    public void makeRequest(int request) {
        synchronized (this) {
            requests.addLast(request);
            notifyAll();
        }
    }

    public boolean isRunning() {
        synchronized (this) {
            return running;
        }
    }
}

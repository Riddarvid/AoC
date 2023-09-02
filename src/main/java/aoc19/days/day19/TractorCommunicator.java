package aoc19.days.day19;

import aoc19.utils.intcode.Communicator;

public class TractorCommunicator extends Communicator {
    public TractorCommunicator(long[] memory) {
        super(memory);
    }

    @Override
    public void output(long val) {
        synchronized (this) {
            outputs.addLast((int)val);
            notifyAll();
        }
    }

    public int getOutput() {
        synchronized (this) {
            while (outputs.isEmpty()) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            int output = outputs.getFirst();
            outputs.removeFirst();
            return output;
        }
    }
}

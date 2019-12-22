package aoc.days.day17;

import aoc.utils.intcode.Communicator;

public class ScaffoldingCommunicator extends Communicator {
    public ScaffoldingCommunicator(long[] memory) {
        super(memory);
    }

    @Override
    public void output(long val) {
        if (val > 150) {
            System.out.println(val);
        } else {
            System.out.print((char)val);
        }
    }
}

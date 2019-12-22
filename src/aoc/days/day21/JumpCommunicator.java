package aoc.days.day21;

import aoc.utils.intcode.Communicator;

public class JumpCommunicator extends Communicator {

    public JumpCommunicator(long[] memory) {
        super(memory);
    }

    @Override
    public void output(long val) {
        if (val > 127) {
            System.out.println(val);
        } else {
            System.out.print((char) val);
        }
    }
}

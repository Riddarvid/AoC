package days.day14.instructions;

import days.day14.Computer;

public interface Mask {
    void apply(Computer computer, long address, long value);
}

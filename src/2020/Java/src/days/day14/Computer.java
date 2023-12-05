package days.day14;

import days.day14.instructions.Mask;

import java.util.HashMap;
import java.util.Map;

public class Computer {
    private Mask mask;
    private final Map<Long, Long> memory;

    public Computer() {
        memory = new HashMap<>();
    }

    public void setMask(Mask mask) {
        this.mask = mask;
    }

    public void updateMemory(long address, long value) {
        mask.apply(this, address, value);
    }

    public long getSum() {
        long sum = 0;
        for (long address : memory.keySet()) {
            sum += memory.get(address);
        }
        return sum;
    }

    public void putMemory(long address, long value) {
        memory.put(address, value);
    }
}

package days.day14.instructions;

import days.day14.Computer;

public class MaskV1 implements Mask {
    private final long onMask;
    private final long offMask;

    public MaskV1(String mask) {
        onMask = parseOnMask(mask);
        offMask = parseOffMask(mask);
    }

    private long parseOnMask(String mask) {
        long onMask = 0;
        for (int i = 0; i < mask.length(); i++) {
            if (mask.charAt(i) == '1') {
                onMask += (long)Math.pow(2, (35 - i));
            }
        }
        return onMask;
    }

    private long parseOffMask(String mask) {
        long offMask = 0;
        for (int i = 0; i < mask.length(); i++) {
            if (mask.charAt(i) != '0') {
                offMask += (long)Math.pow(2, (35 - i));
            }
        }
        return offMask;
    }

    public void apply(Computer computer, long address, long value) {
        value = value | onMask;
        value = value & offMask;
        computer.putMemory(address, value);
    }
}

package days.day14.instructions;

import days.day14.Computer;

import java.util.ArrayList;
import java.util.List;

public class MaskV2 implements Mask {
    private final long onMask;
    private final List<Integer> floating;

    public MaskV2(String mask) {
        onMask = parseOnMask(mask);
        floating = parseFloating(mask);
    }

    private List<Integer> parseFloating(String mask) {
        List<Integer> floating = new ArrayList<>();
        for (int i = 0; i < mask.length(); i++) {
            if (mask.charAt(i) == 'X') {
                floating.add(35 - i);
            }
        }
        return floating;
    }

    private long parseOnMask(String mask) {
        long onMask = 0;
        for (int i = 0; i < mask.length(); i++) {
            if (mask.charAt(i) != '0') {
                onMask += (long)Math.pow(2, (35 - i));
            }
        }
        return onMask;
    }

    @Override
    public void apply(Computer computer, long address, long value) {
        List<Long> addresses = getAddresses(address);
        for (long add : addresses) {
            computer.putMemory(add, value);
        }
    }

    private List<Long> getAddresses(long address) {
        address = address | onMask;
        return getAddresses(0, address);
    }

    private List<Long> getAddresses(int index, long address) {
        List<Long> addresses = new ArrayList<>();
        if (index >= floating.size()) {
            addresses.add(address);
            return addresses;
        }
        addresses.addAll(getAddresses(index + 1, address));
        addresses.addAll(getAddresses(index + 1, address - (long)Math.pow(2, floating.get(index))));
        return addresses;
    }
}

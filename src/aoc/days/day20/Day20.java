package aoc.days.day20;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Day20 extends Day {
    private List<Range> blocked;

    public static void main(String[] args) {
        new Day20();
    }

    @Override
    protected void part1() {
        List<Range> compressedBlocked = new ArrayList<>();
        for (Range range : blocked) {
            insertWithCombine(range, compressedBlocked);
        }
        Collections.sort(compressedBlocked);
        System.out.println(compressedBlocked.get(0).getMax() + 1);
        blocked = compressedBlocked;
    }

    private void insertWithCombine(Range range, List<Range> compressedBlocked) {
        boolean changed = true;
        while (changed) {
            changed = false;
            for (Range other : compressedBlocked) {
                if (range.canCombine(other)) {
                    changed = true;
                    range.combine(other);
                    compressedBlocked.remove(other);
                    break;
                }
            }
        }
        compressedBlocked.add(range);
    }

    @Override
    protected void part2() {
        long nAddresses = (blocked.get(blocked.size() - 1).getMax() + 1);
        long nBlocked = 0;
        for (Range range : blocked) {
            nBlocked += range.getSize();
        }
        System.out.println(nAddresses - nBlocked);
    }

    @Override
    protected void setup() {
        blocked = new ArrayList<>();
        for (String s : lines) {
            List<Long> values = InputUtils.getLongs(s);
            blocked.add(new Range(values.get(0), values.get(1)));
        }
    }
}

package days.day9;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.InputMismatchException;
import java.util.LinkedList;
import java.util.List;

public class Day9 extends Day {
    private List<Long> values;
    private long target;

    public static void main(String[] args) {
        new Day9().runAndPrint();
    }

    @Override
    public long part1() {
        target = findInvalid(values);
        return target;
    }

    private long findInvalid(List<Long> values) {
        LinkedList<Long> currentValues = new LinkedList<>();
        for (int i = 0; i < 25; i++) {
            currentValues.add(values.get(i));
        }
        for (int i = 25; i < values.size(); i++) {
            if (!isValid(currentValues, values.get(i))) {
                return values.get(i);
            }
            currentValues.removeFirst();
            currentValues.add(values.get(i));
        }
        throw new InputMismatchException("No invalid value found");
    }

    private boolean isValid(List<Long> values, long target) {
        for (int i = 0; i < values.size(); i++) {
            long current = values.get(i);
            if (current * 2 != target && values.contains(target - current)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public long part2() {
        List<Long> range = findRange(values, target);
        long smallest = range.get(0);
        long largest = range.get(0);
        for (Long n : range) {
            if (n > largest || smallest == largest) {
                largest = n;
            }
            if (n < smallest) {
                smallest = n;
            }
        }
        return smallest + largest;
    }

    private List<Long> findRange(List<Long> values, long target) {
        long sum = values.get(0);
        int highest = 0;
        int lowest = 0;
        while (sum != target) {
            if (sum < target) {
                highest++;
                sum += values.get(highest);
            } else {
                sum -= values.get(lowest);
                lowest++;
            }
        }
        return values.subList(lowest, highest + 1);
    }

    @Override
    public void setup() {
        values = ParsingUtils.stringsToLongs(lines);
    }
}

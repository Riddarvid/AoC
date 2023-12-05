package aoc.days.day15;

public class Advanced implements Strategy {
    @Override
    public long generateNext(long lastValue, int factor, int requirement) {
        long next;
        do {
            next = factor * lastValue;
            next %= 2147483647;
            lastValue = next;
        } while (next % requirement != 0);
        return next;
    }
}

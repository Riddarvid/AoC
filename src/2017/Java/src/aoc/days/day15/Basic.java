package aoc.days.day15;

public class Basic implements Strategy {

    @Override
    public long generateNext(long lastValue, int factor, int requirement) {
        long next = factor * lastValue;
        next %= 2147483647;
        return next;
    }
}

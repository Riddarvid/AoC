package aoc.days.day15;

public interface Strategy {
    long generateNext(long lastValue, int factor, int requirement);
}

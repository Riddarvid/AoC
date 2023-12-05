package aoc.days.day15;

public class Generator {
    private final long startValue;
    private long lastValue;
    private final int factor;
    private Strategy strategy;
    private final int requirement;

    public Generator(int startValue, int factor, int requirement) {
        this.startValue = startValue;
        this.lastValue = startValue;
        this.factor = factor;
        this.requirement = requirement;
        strategy = new Basic();
    }

    public void generateNext() {
        lastValue = strategy.generateNext(lastValue, factor, requirement);
    }

    public long getLast() {
        return lastValue;
    }

    public void reset() {
        lastValue = startValue;
    }

    public void setStrategy(Strategy strategy) {
        this.strategy = strategy;
    }

    @Override
    public String toString() {
        return "" + lastValue;
    }
}

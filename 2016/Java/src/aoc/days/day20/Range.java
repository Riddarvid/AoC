package aoc.days.day20;

import java.util.InputMismatchException;

public class Range implements Comparable<Range> {
    private long min;
    private long max;

    public Range(long min, long max) {
        this.min = min;
        this.max = max;
    }

    public boolean canCombine(Range other) {
        if (other.min <= this.min && other.max >= this.max) {
            return true;
        }
        if (this.min <= other.min && this.max >= other.max) {
            return true;
        }
        if (other.min >= this.min - 1 && other.min <= this.max + 1) {
            return true;
        }
        if (other.max <= this.max + 1 && other.max >= this.min - 1) {
            return true;
        }
        return false;
    }

    public void combine(Range other) {
        if (canCombine(other)) {
            this.min = Math.min(this.min, other.min);
            this.max = Math.max(this.max, other.max);
        } else {
            throw new InputMismatchException();
        }
    }

    public long getSize() {
        return max - min + 1;
    }

    public long getMin() {
        return min;
    }

    public void setMin(long min) {
        this.min = min;
    }

    public long getMax() {
        return max;
    }

    public void setMax(long max) {
        this.max = max;
    }

    @Override
    public String toString() {
        return min + " - " + max;
    }

    @Override
    public int compareTo(Range range) {
        if (range.min - this.min < 0) {
            return 1;
        } else {
            return -1;
        }
    }
}

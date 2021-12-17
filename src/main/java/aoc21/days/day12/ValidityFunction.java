package aoc21.days.day12;

import java.util.Map;

public interface ValidityFunction {
    boolean isValid(Map<Cave, Integer> caveCount);
}

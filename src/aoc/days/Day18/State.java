package aoc.days.Day18;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class State {
    char[][] map;
    List<Integer> iterations;

    public State(char[][] map, Integer i) {
        this.map = map;
        iterations = new ArrayList<>();
        iterations.add(i);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        State state = (State) o;
        for (int i = 0; i < map.length; i++) {
            if (!Arrays.equals(map[i], state.map[i])) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(map);
    }
}

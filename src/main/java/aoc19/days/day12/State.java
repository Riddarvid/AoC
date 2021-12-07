package aoc19.days.day12;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class State {
    private final List<Moon> moons;

    public State(List<Moon> moons) {
        List<Moon> copy = new ArrayList<>();
        for (Moon moon : moons) {
            copy.add(new Moon(moon));
        }
        this.moons = copy;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        State state = (State) o;
        return Objects.equals(moons, state.moons);
    }

    @Override
    public int hashCode() {
        return Objects.hash(moons);
    }
}

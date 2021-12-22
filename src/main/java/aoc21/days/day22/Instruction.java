package aoc21.days.day22;

public class Instruction {
    private final State state;
    private final Cuboid cuboid;

    public Instruction(State state, Cuboid cuboid) {
        this.state = state;
        this.cuboid = cuboid;
    }

    public State getState() {
        return state;
    }

    public Cuboid getCuboid() {
        return cuboid;
    }

    enum State {
        ON, OFF
    }
}

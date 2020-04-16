package aoc.days.day11;

import java.util.*;

public class State {
    private final List<Pair> pairs;
    private final int elevator;

    public State(List<Pair> pairs, int elevator) {
        this.pairs = new ArrayList<>(pairs);
        this.elevator = elevator;
        Collections.sort(this.pairs);
    }

    public Set<State> getNeighbours(int floors) {
        Set<State> neighbours = new HashSet<>();
        if (elevator < floors) {
            neighbours.addAll(getNeighbours(Direction.UP));
        }
        if (elevator > 1) {
            neighbours.addAll(getNeighbours(Direction.DOWN));
        }
        neighbours = getValid(neighbours);
        return neighbours;
    }

    private Set<State> getValid(Set<State> states) {
        Set<State> validStates = new HashSet<>();
        for (State state : states) {
            if (state.isValid()) {
                validStates.add(state);
            }
        }
        return validStates;
    }

    private boolean isValid() {
        for (Pair pair : pairs) {
            if (pair.getMicrochip() == pair.getGenerator()) {
                continue;
            }
            for (Pair other : pairs) {
                if (other.getGenerator() == pair.getMicrochip()) {
                    return false;
                }
            }
        }
        return true;
    }

    private Set<State> getNeighbours(Direction direction) {
        Set<State> movedSingle = moveSingle(pairs, direction);
        Set<State> movedTwice = moveSingle(movedSingle, direction);
        Set<State> neighbours = new HashSet<>();
        neighbours.addAll(movedSingle);
        neighbours.addAll(movedTwice);
        return neighbours;
    }

    private Set<State> moveSingle(Set<State> states, Direction direction) {
        Set<State> newStates = new HashSet<>();
        for (State state : states) {
            newStates.addAll(moveSingle(state, direction));
        }
        return newStates;
    }

    private Set<State> moveSingle(State state, Direction direction) {
        return moveSingle(state.pairs, direction);
    }

    private Set<State> moveSingle(List<Pair> pairs, Direction direction) { // pairs is sorted
        Set<Pair> moved = new HashSet<>();
        Set<State> newStates = new HashSet<>();
        int newElevator = elevator;
        if (direction == Direction.UP) {
            newElevator++;
        } else {
            newElevator--;
        }
        for (Pair pair : pairs) {
            if (moved.contains(pair)) {
                continue;
            }
            moved.add(pair);
            if (pair.getGenerator() == elevator) {
                Pair newPair = pair.moveGenerator(direction);
                List<Pair> newPairs = new ArrayList<>(pairs);
                newPairs.remove(pair);
                newPairs.add(newPair);
                newStates.add(new State(newPairs, newElevator));
            }
            if (pair.getMicrochip() == elevator) {
                Pair newPair = pair.moveMicrochip(direction);
                List<Pair> newPairs = new ArrayList<>(pairs);
                newPairs.remove(pair);
                newPairs.add(newPair);
                newStates.add(new State(newPairs, newElevator));
            }
        }
        return newStates;
    }

    public State getGoalState(int floors) {
        List<Pair> goalPairs = new ArrayList<>();
        for (int i = 0; i < pairs.size(); i++) {
            goalPairs.add(new Pair(floors, floors));
        }
        return new State(goalPairs, floors);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        State state = (State) o;
        return elevator == state.elevator &&
                Objects.equals(pairs, state.pairs);
    }

    @Override
    public int hashCode() {
        return Objects.hash(pairs, elevator);
    }
}

package aoc21.days.day24;

import java.util.Comparator;

public class LargestComparator implements Comparator<State> {
    @Override
    public int compare(State state1, State state2) {
        //return Long.compare(state1.z(), state2.z());
        return Long.compare(Long.parseLong(state2.inputString()), Long.parseLong(state1.inputString()));
    }
}

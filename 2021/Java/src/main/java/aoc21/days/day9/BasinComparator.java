package aoc21.days.day9;

import java.util.Comparator;
import java.util.Set;

public class BasinComparator <T> implements Comparator<Set<T>> {
    @Override
    public int compare(Set<T> o1, Set<T> o2) {
        return o2.size() - o1.size();
    }
}

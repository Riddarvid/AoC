package aoc15.days.day17;

import java.util.List;

public class ContainerCombo implements Comparable<ContainerCombo> {
    private List<Integer> containers;

    public ContainerCombo(List<Integer> containers) {
        this.containers = containers;
    }

    public int getCount() {
        return containers.size();
    }


    @Override
    public int compareTo(ContainerCombo o) {
        return getCount() - o.getCount();
    }
}

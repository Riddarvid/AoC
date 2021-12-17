package aoc21.days.day12;

import aoc.days.Day;

public class Day12 extends Day {
    private Graph graph;

    @Override
    public void setup() {
        graph = new Graph(lines);
    }

    @Override
    public long part1() {
        return graph.getNumberOfPathsImproved(1);
    }

    @Override
    public long part2() {
        return graph.getNumberOfPathsImproved(2);
    }
}

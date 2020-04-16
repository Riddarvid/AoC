package aoc.days.day11;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.*;

public class Day11 extends Day {
    private State startState1;
    private State startState2;
    private int floors;

    public static void main(String[] args) {
        new Day11();
    }

    private int shortest(State startState, int floors) {
        State goalState = startState.getGoalState(floors);
        int distance = 0;
        Set<State> found = new HashSet<>();
        Set<State> toVisit = new HashSet<>();
        found.add(startState);
        toVisit.add(startState);
        while (!toVisit.isEmpty()) {
            Set<State> toVisitNext = new HashSet<>();
            for (State state : toVisit) {
                if (state.equals(goalState)) {
                    return distance;
                }
                Set<State> neighbours = state.getNeighbours(floors);
                for (State neighbour : neighbours) {
                    if (!found.contains(neighbour)) {
                        found.add(neighbour);
                        toVisitNext.add(neighbour);
                    }
                }
            }
            distance++;
            toVisit = toVisitNext;
        }
        throw new InputMismatchException("No valid path exists");
    }

    private long startTime;

    @Override
    protected void part1() {
        startTime = System.nanoTime();
        System.out.println(shortest(startState1, floors));
    }

    @Override
    protected void part2() {
        System.out.println(shortest(startState2, floors));
        System.out.println((System.nanoTime() - startTime) / 1000000000.0);
    }

    @Override
    protected void setup() {
        floors = lines.size();
        Map<String, Pair> startPairs = new HashMap<>();
        for (int i = 0; i < lines.size(); i++) {
            List<String> generators = getGenerators(lines.get(i));
            for (String generator : generators) {
                if (!startPairs.containsKey(generator)) {
                    startPairs.put(generator, new Pair());
                }
                startPairs.get(generator).setGenerator(i + 1);
            }
            List<String> microchips = getMicrochips(lines.get(i));
            for (String microchip : microchips) {
                if (!startPairs.containsKey(microchip)) {
                    startPairs.put(microchip, new Pair());
                }
                startPairs.get(microchip).setMicrochip(i + 1);
            }
        }
        List<Pair> pairs = new ArrayList<>();
        for (String material : startPairs.keySet()) {
            pairs.add(startPairs.get(material));
        }
        startState1 = new State(pairs, 1);
        pairs.add(new Pair(1, 1));
        pairs.add(new Pair(1, 1));
        startState2 = new State(pairs, 1);
    }

    private List<String> getGenerators(String string) {
        return new ArrayList<>(getBefore(string, "generator"));
    }

    private List<String> getMicrochips(String string) {
        List<String> microchips = new ArrayList<>();
        for (String material : getBefore(string, "microchip")) {
            material = InputUtils.getTokens(material, '-').get(0);
            microchips.add(material);
        }
        return microchips;
    }

    private List<String> getBefore(String string, String target) {
        string = string.replace(".", "");
        string = string.replace(",", "");
        List<String> tokens = InputUtils.getTokens(string, ' ');
        List<String> result = new ArrayList<>();
        for (int i = 0; i < tokens.size(); i++) {
            String token = tokens.get(i);
            if (token.equals(target)) {
                result.add(tokens.get(i - 1));
            }
        }
        return result;
    }
}

package aoc21.days.day23;

import aoc.days.Day;

import java.util.*;

public class Day23 extends Day {
    private Burrow startBurrow1;
    private Burrow startBurrow2;

    private Burrow parseBurrow(int startLine) {
        int lineIndex = startLine + 1;
        int endIndex = startLine + 1;
        while (lines.get(endIndex).charAt(3) != '#') {
            endIndex++;
        }
        Character[] corridor = new Character[lines.get(startLine).length() - 2];
        lineIndex++;
        int numberOfRooms = (lines.get(lineIndex).length() - 5) / 2;
        Room[] rooms = new Room[numberOfRooms];
        for (int i = 0; i < numberOfRooms; i++) {
            rooms[i] = parseRoom(lineIndex, i * 2 + 3, endIndex);
        }
        return new Burrow(corridor, rooms);
    }

    private Room parseRoom(int startY, int x, int endY) {
        Character[] amphipods = new Character[endY - startY];
        for (int y = startY; y < endY; y++) {
            amphipods[y - startY] = lines.get(y).charAt(x);
        }
        return new Room(amphipods);
    }

    private Burrow generateGoalBurrow(int startLine) {
        int lineIndex = startLine + 1;
        int endIndex = startLine + 1;
        while (lines.get(endIndex).charAt(3) != '#') {
            endIndex++;
        }
        Character[] corridor = new Character[lines.get(startLine).length() - 2];
        lineIndex++;
        int sizeOfRooms =  endIndex - lineIndex;
        Room[] rooms = new Room[(lines.get(lineIndex).length() - 5) / 2];
        int numberOfRooms = (lines.get(lineIndex).length() - 5) / 2;
        for (int i = 0; i < numberOfRooms; i++) {
            rooms[i] = generateGoalRoom(sizeOfRooms, i);
        }
        return new Burrow(corridor, rooms);
    }

    private Room generateGoalRoom(int sizeOfRooms, int index) {
        Character[] contents = new Character[sizeOfRooms];
        char podChar = (char) ('A' + index);
        for (int i = 0; i < sizeOfRooms; i++) {
            contents[i] = podChar;
        }
        return new Room(contents);
    }

    @Override
    public void setup() {
        startBurrow1 = parseBurrow(0);
        startBurrow2 = parseBurrow(6);
    }

    private int getShortestPath(Burrow startBurrow, Burrow goalBurrow) {
        Map<Burrow, Integer> costMap = new HashMap<>();
        costMap.put(startBurrow, 0);
        PriorityQueue<Burrow> discovered = new PriorityQueue<>(new BurrowComparator(costMap));
        discovered.add(startBurrow);
        Set<Burrow> determined = new HashSet<>();
        while (!discovered.isEmpty()) {
            Burrow current = discovered.poll();
            if (current.equals(goalBurrow)) {
                return costMap.get(current);
            }
            determined.add(current);
            Set<Move> moves = current.getMoves();
            for (Move move : moves) {
                Burrow neighbour = move.getResultBurrow();
                if (determined.contains(neighbour)) {
                    continue;
                }
                int oldCost = costMap.getOrDefault(neighbour, Integer.MAX_VALUE);
                int newCost = costMap.get(current) + move.getCost();
                if (newCost < oldCost) {
                    costMap.put(neighbour, newCost);
                    if (!discovered.contains(neighbour)) {
                        discovered.add(neighbour);
                    }
                }
            }
        }
        throw new IllegalArgumentException();
    }

    @Override
    public long part1() {
        Burrow goalBurrow = generateGoalBurrow(0);
        return getShortestPath(startBurrow1, goalBurrow);
    }

    @Override
    public long part2() {
        Burrow goalBurrow = generateGoalBurrow(6);
        return getShortestPath(startBurrow2, goalBurrow);
    }
}

package aoc21.days.day23;

import aoc.days.Day;

import java.util.*;

public class Day23 extends Day {
    private Burrow startBurrow;

    @Override
    public void setup() {
        Corridor corridor = new Corridor(11);
        Map<Amphipod, Room> rooms = new HashMap<>();
        for (int x = 3; x <= 9; x += 2) {
            char c = lines.get(2).charAt(x);
            Amphipod outer = Amphipod.valueOf("" + c);
            c = lines.get(3).charAt(x);
            Amphipod inner = Amphipod.valueOf("" + c);
            rooms.put(Amphipod.values()[(x - 3) / 2], new Room(x - 1, outer, inner));
        }
        startBurrow = new Burrow(corridor, rooms);
    }

    @Override
    public long part1() {
        Corridor goalCorridor = new Corridor(11);
        Map<Amphipod, Room> goalRooms = new HashMap<>();
        for (int i = 0; i < Amphipod.values().length; i++) {
            Amphipod amphipod = Amphipod.values()[i];
            goalRooms.put(amphipod, new Room(i * 2 + 2, amphipod, amphipod));
        }
        Burrow goalBurrow = new Burrow(goalCorridor, goalRooms);

        Map<Burrow, Integer> gCostMap = new HashMap<>();
        gCostMap.put(startBurrow, 0);
        Map<Burrow, Integer> fCostMap = new HashMap<>();
        fCostMap.put(startBurrow, startBurrow.estimateRemaining());
        PriorityQueue<Burrow> openSet = new PriorityQueue<>(new BurrowComparator(fCostMap));
        openSet.add(startBurrow);
        //Set<Burrow> determined = new HashSet<>();
        while (!openSet.isEmpty()) {
            Burrow current = openSet.poll();
            if (current.equals(goalBurrow)) {
                return gCostMap.get(current);
            }
            //determined.add(current);
            Set<Move> moves = current.getMoves();
            for (Move move : moves) {
                /*if (determined.contains(move.getBurrow())) {
                    continue;
                }*/
                int oldCost = gCostMap.getOrDefault(move.getBurrow(), Integer.MAX_VALUE);
                int newCost = gCostMap.get(current) + move.getCost();
                if (newCost < oldCost) {
                    gCostMap.put(move.getBurrow(), newCost);
                    fCostMap.put(move.getBurrow(), newCost + move.getBurrow().estimateRemaining());
                    if (!openSet.contains(move.getBurrow())) {
                        openSet.add(move.getBurrow());
                    }
                }
            }
        }
        throw new IllegalArgumentException();
    }

    @Override
    public long part2() {
        return 0;
    }
}

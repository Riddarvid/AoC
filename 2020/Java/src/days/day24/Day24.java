package days.day24;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day24 extends Day {
    private List<Instruction> instructions;
    private Set<Tile> startingTiles;

    public static void main(String[] args) {
        new Day24().runAndPrint();
    }

    @Override
    public long part1() {
        startingTiles = new HashSet<>();
        for (Instruction instruction : instructions) {
            Tile tile = instruction.getTile();
            if (startingTiles.contains(tile)) {
                startingTiles.remove(tile);
            } else {
                startingTiles.add(tile);
            }
        }
        return startingTiles.size();
    }

    @Override
    public long part2() {
        Set<Tile> tiles = startingTiles;
        for (int i =0; i < 100; i++) {
            tiles = update(tiles);
        }
        return tiles.size();
    }

    private Set<Tile> update(Set<Tile> tiles) {
        Set<Tile> potentialTiles = new HashSet<>(tiles);
        for (Tile tile : tiles) {
            potentialTiles.addAll(tile.getNeighbours());
        }
        Set<Tile> newTiles = new HashSet<>();
        for (Tile tile : potentialTiles) {
            int count = countNeighbours(tile, tiles);
            if (tiles.contains(tile)) {
                if (count == 1 || count == 2) {
                    newTiles.add(tile);
                }
            } else {
                if (count == 2) {
                    newTiles.add(tile);
                }
            }
        }
        return newTiles;
    }

    private int countNeighbours(Tile tile, Set<Tile> tiles) {
        Set<Tile> neighbours = tile.getNeighbours();
        int count = 0;
        for (Tile neighbour : neighbours) {
            if (tiles.contains(neighbour)) {
                count++;
            }
        }
        return count;
    }

    @Override
    public void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            instructions.add(new Instruction(s));
        }
    }
}

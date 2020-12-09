package days.day3;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.math.MathUtils;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {
    boolean[][] trees;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    public long part1() {
        int collisions = getCollisions(3, 1);
        return collisions;
    }

    private int getCollisions(int dx, int dy) {
        int x = 0;
        int y = 0;
        int collisions = 0;
        while (y < trees.length) {
            if (trees[y][x]) {
                collisions++;
            }
            x = (x + dx) % trees[0].length;
            y += dy;
        }
        return collisions;
    }

    @Override
    public long part2() {
        List<Integer> collisions = new ArrayList<>();
        collisions.add(getCollisions(1, 1));
        collisions.add(getCollisions(3, 1));
        collisions.add(getCollisions(5, 1));
        collisions.add(getCollisions(7, 1));
        collisions.add(getCollisions(1, 2));
        return MathUtils.product(collisions);
    }

    @Override
    public void setup() {
        trees = ParsingUtils.stringsToBoolMatrix(lines, '#');
    }
}

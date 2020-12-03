package days.day3;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.math.MathUtils;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {
    boolean[][] trees;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    protected void part1() {
        int collisions = getCollisions(3, 1);
        System.out.println(collisions);
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
    protected void part2() {
        List<Integer> collisions = new ArrayList<>();
        collisions.add(getCollisions(1, 1));
        collisions.add(getCollisions(3, 1));
        collisions.add(getCollisions(5, 1));
        collisions.add(getCollisions(7, 1));
        collisions.add(getCollisions(1, 2));
        System.out.println(MathUtils.product(collisions));
    }

    @Override
    protected void setup() {
        trees = new boolean[lines.size()][];
        for (int line = 0; line < lines.size(); line++) {
            String s = lines.get(line);
            trees[line] = new boolean[s.length()];
            for (int i = 0; i < s.length(); i++) {
                if (s.charAt(i) == '#') {
                    trees[line][i] = true;
                }
            }
        }
    }
}

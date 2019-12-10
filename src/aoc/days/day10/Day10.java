package aoc.days.day10;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Day10 extends Day {
    private List<Asteroid> asteroids;

    public static void main(String[] args) {
        new Day10();
    }

    @Override
    protected void part1() {
        for (Asteroid asteroid : asteroids) {
            asteroid.calculateLineOfSights(asteroids);
        }
        Queue<Asteroid> pq = new PriorityQueue<>();
        pq.addAll(asteroids);
        System.out.println(pq.poll().getLineOfSights());
    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        asteroids = new ArrayList<>();
        for (int row = 0; row < lines.size(); row++) {
            String s = lines.get(row);
            for (int pos = 0; pos < s.length(); pos++) {
                if (s.charAt(pos) == '#') {
                    asteroids.add(new Asteroid(pos, row));
                }
            }
        }
    }
}

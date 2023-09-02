package aoc19.days.day10;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Day10 extends Day {
    private List<Asteroid> asteroids;
    private Asteroid laser;

    public static void main(String[] args) {
        new Day10();
    }

    @Override
    protected void part1() {
        Asteroid best = asteroids.get(0);
        for (Asteroid asteroid : asteroids) {
            List<Asteroid> others = new ArrayList<>(asteroids);
            others.remove(asteroid);
            asteroid.calculateNLineOfSight(others);
            if (asteroid.getNLineOfSight() > best.getNLineOfSight()) {
                best = asteroid;
            }
        }
        laser = best;
        asteroids.remove(laser);
        System.out.println(best.getNLineOfSight());
    }

    @Override
    protected void part2() {
        int i = 0;
        Queue<Asteroid> visible = new PriorityQueue<>();
        while (i < 199) {
            visible = new PriorityQueue<>(new AsteroidComparator(laser));
            visible.addAll(getVisible(asteroids));
            asteroids.removeAll(visible);
            while (i < 199 && !visible.isEmpty()) {
                i++;
                visible.poll();
            }
        }
        System.out.println(visible.peek().getX() * 100 + visible.peek().getY());
    }

    private List<Asteroid> getVisible(List<Asteroid> asteroids) {
        List<Asteroid> visible = new ArrayList<>();
        for (Asteroid asteroid : asteroids) {
            List<Asteroid> others = new ArrayList<>(asteroids);
            others.remove(asteroid);
            if (isVisible(asteroid, others)) {
                visible.add(asteroid);
            }
        }
        return visible;
    }

    private boolean isVisible(Asteroid asteroid, List<Asteroid> others) {
        for (Asteroid other : others) {
            if (laser.angleTo(asteroid).equals(laser.angleTo(other)) && laser.distanceTo(other) < laser.distanceTo(asteroid)) {
                return false;
            }
        }
        return true;
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

package aoc.days.day20;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.ArrayList;
import java.util.List;

public class Day20 extends Day {
    private List<Particle> particles;

    public static void main(String[] args) {
        new Day20();
    }

    @Override
    protected void part1() {
        List<Particle> lowestParticles = getLowestPosition(getLowestSpeed(getLowestAcceleration(particles)));
        System.out.println(lowestParticles.get(0).getId());
    }

    private void run(int steps) {
        for (int i = 0; i < steps; i++) {
            step();
        }
    }

    private void step() {
        for (Particle p : particles) {
            p.changeVelocity();
            p.changePosition();
        }
        List<Vector> collisions = new ArrayList<>();
        List<Vector> positions = new ArrayList<>();
        for (Particle p : particles) {
            if (positions.contains(p.getPosition())) {
                collisions.add(p.getPosition());
            } else {
                positions.add(p.getPosition());
            }
        }
        List<Particle> toRemove = new ArrayList<>();
        for (Particle p : particles) {
            if (collisions.contains(p.getPosition())) {
                toRemove.add(p);
            }
        }
        particles.removeAll(toRemove);
    }

    private List<Particle> getLowestAcceleration(List<Particle> particles) {
        int lowestValue = manhattan(particles.get(0).getAcceleration());
        for (Particle p : particles) {
            if (manhattan(p.getAcceleration()) < lowestValue) {
                lowestValue = manhattan(p.getAcceleration());
            }
        }
        List<Particle> lowestParticles = new ArrayList<>();
        for (Particle p : particles) {
            if (manhattan(p.getAcceleration()) == lowestValue) {
                lowestParticles.add(p);
            }
        }
        return lowestParticles;
    }

    private List<Particle> getLowestSpeed(List<Particle> particles) {
        int lowestValue = manhattan(particles.get(0).getVelocity());
        for (Particle p : particles) {
            if (manhattan(p.getVelocity()) < lowestValue) {
                lowestValue = manhattan(p.getVelocity());
            }
        }
        List<Particle> lowestParticles = new ArrayList<>();
        for (Particle p : particles) {
            if (manhattan(p.getVelocity()) == lowestValue) {
                lowestParticles.add(p);
            }
        }
        return lowestParticles;
    }

    private List<Particle> getLowestPosition(List<Particle> particles) {
        int lowestValue = manhattan(particles.get(0).getPosition());
        for (Particle p : particles) {
            if (manhattan(p.getPosition()) < lowestValue) {
                lowestValue = manhattan(p.getPosition());
            }
        }
        List<Particle> lowestParticles = new ArrayList<>();
        for (Particle p : particles) {
            if (manhattan(p.getPosition()) == lowestValue) {
                lowestParticles.add(p);
            }
        }
        return lowestParticles;
    }

    private int manhattan(Vector v) {
        int a = Math.abs(v.getX());
        int b = Math.abs(v.getY());
        int c = Math.abs(v.getZ());
        return a + b + c;
    }

    @Override
    protected void part2() {
        run(100000);
        System.out.println(particles.size());
    }

    @Override
    protected void setup() {
        particles = new ArrayList<>();
        for (int i = 0; i < lines.size(); i++) {
            String s = lines.get(i);
            List<Integer> values = InputUtilities.getIntsNegative(s);
            Vector position = new Vector(values.get(0), values.get(1), values.get(2));
            Vector velocity = new Vector(values.get(3), values.get(4), values.get(5));
            Vector acceleration = new Vector(values.get(6), values.get(7), values.get(8));
            particles.add(new Particle(i, position, velocity, acceleration));
        }
    }
}

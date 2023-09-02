package aoc19.days.day12;

import aoc19.utils.input.InputUtils;
import aoc19.utils.math.MathUtils;
import riddarvid.aoc.days.Day;

import java.util.*;


public class Day12 extends Day {
    private List<Moon> moons1;
    private List<Moon> moons2;

    public static void main(String[] args) {
        new Day12();
    }

    @Override
    protected void part1() {
        for (int i = 0; i < 1000; i++) {
            step(moons1);
        }
        System.out.println(getTotalEnergy());
    }

    private long getTotalEnergy() {
        long total = 0;
        for (Moon moon : moons1) {
            total += moon.getTotalEnergy();
        }
        return total;
    }

    private void step(List<Moon> moons) {
        for (Moon moon : moons) {
            moon.updateVelocity(moons);
        }
        for (Moon moon : moons) {
            moon.updatePosition();
        }
    }

    @Override
    protected void part2() {
        long cycleX = findCycle(moons2, 'x');
        long cycleY = findCycle(moons2, 'y');
        long cycleZ = findCycle(moons2, 'z');
        long period = MathUtils.lcm(MathUtils.lcm(cycleX, cycleY), cycleZ);
        System.out.println(period);
    }

    private List<Tuple> createState(List<Moon> moons, char axis) {
        List<Tuple> state = new ArrayList<>();
        for (Moon moon : moons) {
            switch (axis) {
                case 'x':
                    state.add(new Tuple(moon.getPosition().getX(), moon.getVelocity().getX()));
                    break;
                case 'y':
                    state.add(new Tuple(moon.getPosition().getY(), moon.getVelocity().getY()));
                    break;
                case 'z':
                    state.add(new Tuple(moon.getPosition().getZ(), moon.getVelocity().getZ()));
                    break;
                default:
                    throw new InputMismatchException();
            }
        }
        return state;
    }

    private int findCycle(List<Moon> moons, char axis) {
        List<Tuple> initial = createState(moons, axis);
        int i = 0;
        while (true) {
            i++;
            step(moons);
            List<Tuple> state = createState(moons, axis);
            if (state.equals(initial)) {
                break;
            }
        }
        return i;
    }

    @Override
    protected void setup() {
        moons1 = new ArrayList<>();
        moons2 = new ArrayList<>();
        for (String s : lines) {
            List<Long> coords = InputUtils.getIntsNegative(s);
            long x = coords.get(0);
            long y = coords.get(1);
            long z = coords.get(2);
            moons1.add(new Moon(x, y, z));
            moons2.add(new Moon(x, y, z));
        }
    }
}

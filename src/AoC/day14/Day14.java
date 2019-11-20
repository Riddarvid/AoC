package AoC.day14;

import AoC.Day;
import AoC.FileUtilities;

import java.util.ArrayList;
import java.util.List;

public class Day14 extends Day {
    List<Reindeer> reindeer;

    public static void main(String[] args) {
        new Day14();
    }

    private void moveAll() {
        for (Reindeer r : reindeer) {
            r.move();
        }
    }

    private Reindeer getFurthest() {
        Reindeer fastest = reindeer.get(0);
        for (Reindeer r : reindeer) {
            if (r.getDistanceTraveled() > fastest.getDistanceTraveled()) {
                fastest = r;
            }
        }
        return fastest;
    }

    @Override
    protected void part1() {
        for (int i = 0; i < 2503; i++) {
            moveAll();
        }
        Reindeer fastest = getFurthest();
        System.out.println(fastest.getDistanceTraveled());
    }

    @Override
    protected void part2() {
        resetAll();
        for (int i = 0; i < 2503; i++) {
            moveAll();
            getFurthest().incPoints();
        }
        System.out.println(getBest().getPoints());
    }

    private Reindeer getBest() {
        Reindeer best = reindeer.get(0);
        for (Reindeer r : reindeer) {
            if (r.getPoints() > best.getPoints()) {
                best = r;
            }
        }
        return best;
    }

    private void resetAll() {
        for (Reindeer r : reindeer) {
            r.reset();
        }
    }

    @Override
    protected void setup() {
        reindeer = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = FileUtilities.getTokens(s, ' ');
            Reindeer current = new Reindeer(tokens.get(0), Integer.parseInt(tokens.get(3)), Integer.parseInt(tokens.get(6)), Integer.parseInt(tokens.get(13)));
            reindeer.add(current);
        }
    }
}

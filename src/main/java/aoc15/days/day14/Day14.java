package aoc15.days.day14;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day14 extends Day {
    List<Reindeer> reindeer;

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
    public long part1() {
        for (int i = 0; i < 2503; i++) {
            moveAll();
        }
        Reindeer fastest = getFurthest();
        return fastest.getDistanceTraveled();
    }

    @Override
    public long part2() {
        resetAll();
        for (int i = 0; i < 2503; i++) {
            moveAll();
            getFurthest().incPoints();
        }
        return getBest().getPoints();
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
    public void setup() {
        reindeer = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            Reindeer current = new Reindeer(tokens.get(0), Integer.parseInt(tokens.get(3)), Integer.parseInt(tokens.get(6)), Integer.parseInt(tokens.get(13)));
            reindeer.add(current);
        }
    }
}

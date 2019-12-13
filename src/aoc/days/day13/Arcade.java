package aoc.days.day13;

import aoc.utils.geometry.Point2D;
import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class Arcade implements Controller {
    private long[] program;
    private int outputPhase;
    private Map<Point2D, Tile> objects;
    private long xToFill;
    private long yToFill;
    private long score;

    public Arcade(long[] program) {
        this.program = Arrays.copyOf(program, program.length);
        objects = new HashMap<>();
        outputPhase = 0;
        score = 0;
    }

    public void run() {
        IntcodeComputer ic = new IntcodeComputer(this, program);
        ic.execute(1000/60);
    }

    @Override
    public long getInput() { //TODO
        return 0;
    }

    @Override
    public void output(long val) {
        switch (outputPhase) {
            case 0:
                xToFill = val;
                break;
            case 1:
                yToFill = val;
                break;
            case 2:
                if (xToFill == -1 && yToFill == 0) {
                    score = val;
                } else {
                    objects.put(new Point2D(xToFill, yToFill), Tile.values()[(int)val]);
                }
                break;
        }
        outputPhase = (outputPhase + 1) % 3;
    }

    public int getNumberOf(Tile type) {
        int sum = 0;
        for (Tile t : objects.values()) {
            if (t == type) {
                sum++;
            }
        }
        return sum;
    }

    public long score() {
        return score;
    }
}

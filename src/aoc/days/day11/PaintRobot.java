package aoc.days.day11;

import aoc.utils.geometry.Direction;
import aoc.utils.geometry.Point2D;
import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;

import java.util.HashMap;
import java.util.Map;

public class PaintRobot implements Controller {
    private Map<Point2D, Boolean> painted;
    private Point2D position;
    private Direction direction;
    private long[] program;
    private boolean shouldPaint;

    public PaintRobot(long[] program) {
        painted = new HashMap<>();
        position = new Point2D(0, 0);
        direction = Direction.NORTH;
        this.program = program;
        shouldPaint = true;
    }

    public void run() {
        IntcodeComputer ic = new IntcodeComputer(this, program, false);
        ic.execute();
    }

    @Override
    public long getInput() {
        if (painted.get(position) == null || !painted.get(position)) {
            return 0;
        } else {
            return 1;
        }
    }

    @Override
    public void output(long val) {
        if (shouldPaint) {
            if (val == 0) {
                painted.put(position, false);
            } else {
                painted.put(position, true);
            }
            shouldPaint = false;
        } else { //shouldMove
            if (val == 0) {
                turnLeft();
            } else {
                turnRight();
            }
            move();
            shouldPaint = true;
        }
    }

    private void move() {
        switch (direction) {
            case EAST:
                position = position.moveBy(1, 0);
                break;
            case WEST:
                position = position.moveBy(-1, 0);
                break;
            case NORTH:
                position = position.moveBy(0, -1);
                break;
            case SOUTH:
                position = position.moveBy(0, 1);
                break;
        }
    }

    private void turnRight() {
        direction = Direction.values()[(direction.ordinal() + 1) % Direction.values().length];
    }

    private void turnLeft() {
        direction = Direction.values()[(direction.ordinal() + Direction.values().length - 1) % Direction.values().length];
    }

    public int getNPainted() {
        return painted.size();
    }

    public void set(int x, int y, boolean color) {
        painted.put(new Point2D(x, y), color);
    }

    public Map<Point2D, Boolean> getPainted() {
        return painted;
    }
}

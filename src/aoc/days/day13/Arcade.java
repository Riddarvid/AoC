package aoc.days.day13;

import aoc.days.day13.view.View;
import aoc.utils.geometry.Point2D;
import aoc.utils.intcode.Controller;
import aoc.utils.intcode.IntcodeComputer;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class Arcade implements Controller {
    private long[] program;
    private int outputPhase;
    private final Map<Point2D, Tile> objects;
    private long xToFill;
    private long yToFill;
    private long score;
    private View view;
    private boolean shouldDelay;
    private boolean leftPressed;
    private boolean rightPressed;
    private Point2D ballPos;
    private Point2D paddlePos;

    public Arcade(long[] program, boolean part2) {
        this.program = Arrays.copyOf(program, program.length);
        this.shouldDelay = part2;
        /*if (part2) {
            KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(keyEvent -> {
                synchronized (Arcade.class) {
                    switch (keyEvent.getID()) {
                        case KeyEvent.KEY_PRESSED:
                            if (keyEvent.getKeyCode() == KeyEvent.VK_A) {
                                leftPressed = true;
                            } else if (keyEvent.getKeyCode() == KeyEvent.VK_D) {
                                rightPressed = true;
                            }
                            break;

                        case KeyEvent.KEY_RELEASED:
                            if (keyEvent.getKeyCode() == KeyEvent.VK_A) {
                                leftPressed = false;
                            } else if (keyEvent.getKeyCode() == KeyEvent.VK_D) {
                                rightPressed = false;
                            }
                            break;
                    }
                    return false;
                }
            });
        }*/
        ballPos = new Point2D(0, 0);
        paddlePos = new Point2D(0, 0);
        objects = new HashMap<>();
        outputPhase = 0;
        score = 0;
    }

    public void run() {
        IntcodeComputer ic = new IntcodeComputer(this, program, shouldDelay);
        ic.execute();
    }

    @Override
    public long getInput() {
        /*if (leftPressed) {
            return -1;
        } else if (rightPressed) {
            return 1;
        }
        return 0;*/
        if (paddlePos.getX() < ballPos.getX()) {
            return 1;
        } else if (paddlePos.getX() > ballPos.getX()) {
            return -1;
        }
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
                    synchronized (objects) {
                        Tile tile = Tile.values()[(int) val];
                        objects.put(new Point2D(xToFill, yToFill), tile);
                        if (tile == Tile.PADDLE) {
                            paddlePos = new Point2D(xToFill, yToFill);
                        } else if (tile == Tile.BALL) {
                            ballPos = new Point2D(xToFill, yToFill);
                        }
                    }
                    if (view != null) {
                        view.update();
                    }
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

    public void activateView() {
        view = new View(objects);
    }
}

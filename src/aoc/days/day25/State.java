package aoc.days.day25;

public class State {
    private final int writeIfZero;
    private final int moveIfZero;
    private final char nextStateIfZero;

    private final int writeIfOne;
    private final int moveIfOne;
    private final char nextStateIfOne;

    public State(int writeIfZero, int moveIfZero, char nextStateIfZero, int writeIfOne, int moveIfOne, char nextStateIfOne) {
        this.writeIfZero = writeIfZero;
        this.moveIfZero = moveIfZero;
        this.nextStateIfZero = nextStateIfZero;
        this.writeIfOne = writeIfOne;
        this.moveIfOne = moveIfOne;
        this.nextStateIfOne = nextStateIfOne;
    }

    public int toWrite(int val) {
        if (val == 0) {
            return writeIfZero;
        }
        return writeIfOne;
    }

    public int toMove(int val) {
        if (val == 0) {
            return moveIfZero;
        }
        return moveIfOne;
    }

    public char nextState(int val) {
        if (val == 0) {
            return nextStateIfZero;
        }
        return nextStateIfOne;
    }
}

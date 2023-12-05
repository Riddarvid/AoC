package aoc.days.day15;

public class Disc {
    private final int id;
    private final int nPositions;
    private int position;
    private final int goalPosition;

    public Disc(int id, int nPositions, int position) {
        this.id = id;
        this.nPositions = nPositions;
        this.position = position;
        if (nPositions - id < 0) {
            goalPosition = 2*nPositions - id;
        } else {
            goalPosition = nPositions - id;
        }
    }

    @Override
    public String toString() {
        return "Disc " + id + " at " + position + ", goal at " + goalPosition;
    }

    public int getId() {
        return id;
    }

    public int getnPositions() {
        return nPositions;
    }

    public int getPosition() {
        return position;
    }

    public void move(int time) {
        position = (position + time) % nPositions;
    }

    public boolean isDone() {
        return position == goalPosition;
    }
}

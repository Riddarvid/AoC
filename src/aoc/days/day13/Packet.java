package aoc.days.day13;

public class Packet {
    private int position;
    private final int delay;

    public Packet(int delay) {
        position = 0;
        this.delay = delay;
    }

    public void move() {
        position++;
    }

    public int getDelay() {
        return delay;
    }

    public int getPosition() {
        return position;
    }
}

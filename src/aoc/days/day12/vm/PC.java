package aoc.days.day12.vm;

public class PC {
    private int position;

    public PC() {
        position = 0;
    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    public void incPosition() {
        position++;
    }

    @Override
    public String toString() {
        return position + "";
    }
}

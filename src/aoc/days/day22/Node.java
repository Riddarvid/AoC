package aoc.days.day22;

public class Node {
    private final int size;
    private int used;

    public Node(int size, int used) {
        this.size = size;
        this.used = used;
    }

    public int getSize() {
        return size;
    }

    public int getUsed() {
        return used;
    }

    public int getAvail() {
        return size - used;
    }

    @Override
    public String toString() {
        return "Size: " + size + ", Used: " + used;
    }

    public boolean fitsIn(Node b) {
        return used <= b.getAvail();
    }

    public boolean isWall() {
        return size > 100;
    }

    public boolean isEmpty() {
        return used == 0;
    }
}

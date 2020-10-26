package aoc.days.day3;

import java.util.List;

public class Claim {
    private final int id;
    private final int offsetX;
    private final int offsetY;
    private final int width;
    private final int height;

    public Claim(int id, int offsetX, int offsetY, int width, int height) {
        this.id = id;
        this.offsetX = offsetX;
        this.offsetY = offsetY;
        this.width = width;
        this.height = height;
    }

    public Claim(List<Integer> integers) {
        id = integers.get(0);
        offsetX = integers.get(1);
        offsetY = integers.get(2);
        width = integers.get(3);
        height = integers.get(4);
    }

    public int getId() {
        return id;
    }

    public int getOffsetX() {
        return offsetX;
    }

    public int getOffsetY() {
        return offsetY;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }
}

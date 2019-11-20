package AoC.Day3;

public class Claim {
    private int id;
    private int offsetX;
    private int offsetY;
    private int width;
    private int height;

    public Claim(int id, int offsetX, int offsetY, int width, int height) {
        this.id = id;
        this.offsetX = offsetX;
        this.offsetY = offsetY;
        this.width = width;
        this.height = height;
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

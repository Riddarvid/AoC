package AoC.day3;

public class Actor {
    private int x;
    private int y;

    public Actor(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Actor() {
        this(0, 0);
    }

    void up() {
        y--;
    }

    void down() {
        y++;
    }

    void left() {
        x--;
    }

    void right() {
        x++;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }
}

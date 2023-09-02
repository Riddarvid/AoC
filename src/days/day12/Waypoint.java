package days.day12;

public class Waypoint {
    private int x = 10;
    private int y = 1;

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public void rotateLeft() {
        int newY = x;
        int newX = -y;
        x = newX;
        y = newY;
    }

    public void move(int dx, int dy) {
        x += dx;
        y += dy;
    }
}

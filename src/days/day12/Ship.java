package days.day12;

public class Ship {
    private int x = 0;
    private int y = 0;
    private Direction direction = Direction.EAST;
    private Waypoint waypoint = new Waypoint();

    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    public Direction getDirection() {
        return direction;
    }

    public void setDirection(Direction direction) {
        this.direction = direction;
    }


    public int getWaypointX() {
        return waypoint.getX();
    }

    public int getWaypointY() {
        return waypoint.getY();
    }

    public void move(int dx, int dy) {
        x += dx;
        y += dy;
    }

    public void rotateWaypointLeft(int degrees) {
        while (degrees > 0) {
            waypoint.rotateLeft();
            degrees -= 90;
        }
    }

    public void moveWaypoint(int dx, int dy) {
        waypoint.move(dx, dy);
    }
}

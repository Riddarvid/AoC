package days.day12;

import riddarvid.aoc.math.geometry.Point;
import riddarvid.aoc.math.geometry.Vector;

public class Ship {
    private Point position = new Point();
    private Vector heading = new Vector(1, 0);
    private Vector waypoint = new Vector(10, 1);

    public Point getPosition() {
        return position;
    }

    public void moveBy(int x, int y) {
        position = position.moveBy(x, y);
    }

    public void moveForward(int value) {
        position = position.moveBy(heading.scaleBy(value));
    }

    public void rotateHeadingLeft(int value) {
        while (value > 0) {
            rotateHeadingLeft();
            value -= 90;
        }
    }

    private void rotateHeadingLeft() {
        heading = heading.rotateLeft();
    }

    public void moveWaypointBy(int x, int y) {
        waypoint = waypoint.add(x, y);
    }

    public void rotateWaypointLeft(int value) {
        while (value > 0) {
            rotateWaypointLeft();
            value -= 90;
        }
    }

    private void rotateWaypointLeft() {
        waypoint = waypoint.rotateLeft();
    }

    public void moveToWaypoint(int value) {
        position = position.moveBy(waypoint.scaleBy(value));
    }
}

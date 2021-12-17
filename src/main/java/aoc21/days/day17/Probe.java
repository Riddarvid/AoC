package aoc21.days.day17;

import aoc.math.geometry.Point;
import aoc.math.geometry.Vector;

public class Probe {
    private Point position;
    private Vector velocity;

    public Probe(Vector velocity) {
        this.velocity = velocity;
        position = new Point();
    }

    public void move() {
        position = position.moveBy(velocity);
        if (velocity.getX() > 0) {
            velocity = velocity.add(-1, 0);
        } else if (velocity.getX() < 0) {
            velocity = velocity.add(1, 0);
        }
        velocity = velocity.add(0, -1);
    }

    public Point getPosition() {
        return position;
    }

    public int getX() {
        return position.getX();
    }

    public int getY() {
        return position.getY();
    }

    public Vector getVelocity() {
        return velocity;
    }

    public int getDX() {
        return velocity.getX();
    }

    public int getDY() {
        return velocity.getY();
    }
}

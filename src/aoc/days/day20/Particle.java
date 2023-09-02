package aoc.days.day20;

import javax.lang.model.element.VariableElement;

public class Particle {
    private int id;
    private Vector position;
    private Vector velocity;
    private Vector acceleration;

    public Particle(int id, Vector position, Vector velocity, Vector acceleration) {
        this.id = id;
        this.position = position;
        this.velocity = velocity;
        this.acceleration = acceleration;
    }

    public void changeVelocity() {
        velocity = velocity.add(acceleration);
    }

    public void changePosition() {
        position = position.add(velocity);
    }

    public Vector getAcceleration() {
        return acceleration;
    }

    public int getId() {
        return id;
    }

    public Vector getPosition() {
        return position;
    }

    public Vector getVelocity() {
        return velocity;
    }
}

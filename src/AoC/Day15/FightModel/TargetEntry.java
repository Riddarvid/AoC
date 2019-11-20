package AoC.Day15.FightModel;

public class TargetEntry implements IPositionable {
    private Point target;
    private int distance;

    public TargetEntry(Point target, int distance) {
        this.target = target;
        this.distance = distance;
    }

    public Point getTarget() {
        return target;
    }

    public int getDistance() {
        return distance;
    }

    @Override
    public int getX() {
        return target.getX();
    }

    @Override
    public int getY() {
        return target.getY();
    }

    @Override
    public String toString() {
        return target.getX() + "," + target.getY();
    }
}

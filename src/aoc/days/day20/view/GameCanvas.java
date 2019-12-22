package aoc.days.day20.view;

import aoc.days.day20.Location;
import aoc.utils.geometry.Point2D;

import java.awt.*;
import java.util.Set;

public class GameCanvas extends Canvas {
    private Set<Location> explored;
    private Set<Location> foundLastCycle;
    private Set<Point2D> points;
    private final int width;
    private final int height;
    private final int side;

    public GameCanvas(Set<Location> explored, Set<Location> foundLastCycle, Set<Point2D> points, int width, int height, int side) {
        this.explored = explored;
        this.foundLastCycle = foundLastCycle;
        this.points = points;
        this.width = width;
        this.height = height;
        this.side = side;
        setBackground(Color.WHITE);
        setSize(width * side + 100, height * side + side);
    }

    public void setExplored(Set<Location> explored) {
        this.explored = explored;
    }

    public void setFoundLastCycle(Set<Location> foundLastCycle) {
        this.foundLastCycle = foundLastCycle;
    }

    public void setPoints(Set<Point2D> points) {
        this.points = points;
    }

    @Override
    public void update(Graphics g) {
        paint(g);
    }

    @Override
    public void paint(Graphics g) {
        synchronized (this) {
            g.clearRect(0, 0, 640, 480);
            g.setColor(Color.BLACK);
            for (Point2D point : points) {
                g.fillRect((int) point.getX() * side , (int) point.getY() * side, side, side);
            }
            g.setColor(Color.GREEN);
            for (Location location : explored) {
                Point2D point = location.getPoint();
                g.fillRect((int) point.getX() * side , (int) point.getY() * side, side, side);
            }
            for (Location location : foundLastCycle) {
                Point2D point = location.getPoint();
                g.setColor(Color.RED);
                g.fillRect((int) point.getX() * side, (int) point.getY() * side, side, side);
                g.setColor(Color.BLACK);
                g.drawString("" + location.getDepth(), (int) point.getX() * side + side / 2, (int) point.getY() * side + side);
            }
        }
    }
}
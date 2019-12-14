package aoc.days.day13.view;

import aoc.days.day13.Tile;
import aoc.utils.geometry.Point2D;

import java.awt.*;
import java.util.Map;

public class GameCanvas extends Canvas {
    private final Map<Point2D, Tile> objects;
    private int side = 20;

    public GameCanvas(Map<Point2D, Tile> objects) {
        this.objects = objects;
        setBackground(Color.BLACK);
        setSize(1000, 600);
    }

    @Override
    public void update(Graphics g) {
        paint(g);
    }

    @Override
    public void paint(Graphics g) {
        synchronized (objects) {
            g.clearRect(0, 0, 640, 480);
            for (Point2D point : objects.keySet()) {
                switch (objects.get(point)) {
                    case BLOCK:
                        g.setColor(Color.RED);
                        break;
                    case EMPTY:
                        g.setColor(Color.BLACK);
                        break;
                    case BALL:
                        g.setColor(Color.BLUE);
                        break;
                    case WALL:
                        g.setColor(Color.GREEN);
                        break;
                    case PADDLE:
                        g.setColor(Color.ORANGE);
                        break;
                }
                g.fillRect((int) point.getX() * side + 50, (int) point.getY() * side + 50, side, side);
            }
        }
    }
}

package aoc.days.Day6;

import javax.swing.*;
import java.awt.*;
import java.util.List;

public class DrawPanel extends JPanel {
    private List<Point> points;
    private int width;
    private int height;
    private int minX;
    private int minY;

    public DrawPanel(List<Point> points, int minX, int minY, int width, int height) {
        this.points = points;
        this.minX = minX;
        this.minY = minY;
        this.setDoubleBuffered(true);
        this.width = width;
        this.height = height;
        this.setPreferredSize(new Dimension(width, height));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        for (Point p : points) {
            g.setColor(p.getColor());
            g.fillRect(p.getX() - minX, p.getY() - minY, 1, 1);
        }
    }
}

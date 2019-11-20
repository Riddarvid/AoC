package AoC.Day10;

import javax.swing.*;
import java.awt.*;

public class DrawPanel extends JPanel {
    private final Model model;

    public DrawPanel(Model model, int width, int height) {
        this.model = model;
        this.setDoubleBuffered(true);
        this.setPreferredSize(new Dimension(width, height));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.setColor(Color.RED);
        for (Point p : model.getPoints()) {
            double minX = model.getMinX();
            double maxX = model.getMaxX();
            double minY = model.getMinY();
            double maxY = model.getMaxY();

            double x = p.getX() - minX;
            double y = p.getY() - minY;
            double kx = (maxX - minX)/1000;
            double ky = (maxY - minY)/1000;
            double k = Math.max(kx, ky);
            /*if (x > 1000 || y > 1000) {
                x = x/100;
                y = y/100;
            }*/
            g.fillRect((int)(x/k), (int)(y/k), 5, 5);
        }
    }

    private double getDiffX(Point[] points) {
        return 0;
    }

    private double getDiffY(Point[] points) {
        return 0;
    }
}

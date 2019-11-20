package AoC.Day20;

import javax.swing.*;
import java.awt.*;
import java.util.List;

public class BaseView extends JFrame {
    Day20 model;
    int size = 1000;
    int space = 3;
    int sizeOfPoint = 2;
    int distanceBetweenCenter = 7;

    public BaseView(Day20 model) {
        this.model = model;
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(0, 0, 1800, 1000);
        setBackground(Color.BLACK);
        setVisible(true);
        repaint();
    }

    public void update() {
        repaint();
    }

    @Override
    public void paint(Graphics g) {
        if (model.redrawGraph) {
            g.setColor(Color.GREEN);
            Point start = model.discovered.get(0);
            g.fillRect(start.x * distanceBetweenCenter, start.y * distanceBetweenCenter, sizeOfPoint, sizeOfPoint);
            for (int i = 1; i < model.discovered.size(); i++) {
                Point p = model.discovered.get(i);
                g.setColor(Color.WHITE);
                //g.fillRect(p.x * distanceBetweenCenter, p.y * distanceBetweenCenter, sizeOfPoint, sizeOfPoint);
                g.setColor(Color.RED);
                drawConnections(g, p);
            }
        } /*else if (!model.toBeDrawn.isEmpty() && !model.done){
            g.setColor(Color.WHITE);
            Point p = model.toBeDrawn.get(model.toBeDrawn.size() - 1);
            drawPath(g, p);
            g.setColor(Color.GREEN);
            Point start = model.discovered.get(0);
            g.fillRect(start.x * distanceBetweenCenter, start.y * distanceBetweenCenter, 4, 4);
            //g.setColor(Color.YELLOW);
            //g.fillRect(p.x * distanceBetweenCenter, p.y * distanceBetweenCenter, 4, 4);
        }*/
        if (model.done) {
            Point p = model.longest;
            g.setColor(Color.RED);
            g.fillRect(p.x * distanceBetweenCenter, p.y * distanceBetweenCenter, 4, 4);
            g.setColor(Color.BLUE);
            drawPath(g, p);
        }
    }

    private void drawPath(Graphics g, Point p) {
        while (p.previous != null) {
            drawLine(g, p, p.previous);
            p = p.previous;
            if (model.done) {
                /*try {
                    Thread.sleep(10);
                } catch (InterruptedException e) {

                }*/
            }
        }
    }

    private void drawLine(Graphics g, Point a, Point b) {
        g.drawLine(midX(a), midY(a), midX(b), midY(b));
    }

    private void drawConnections(Graphics g, Point p) {
        if (p.north != null) {
            drawLine(g, p, p.north);
        }
        if (p.east != null) {
            drawLine(g, p, p.east);
        }
        if (p.south != null) {
            drawLine(g, p, p.south);
        }
        if (p.west != null) {
            drawLine(g, p, p.west);
        }
        /*if (p.north != null) {
            g.fillRect(p.x * distanceBetweenCenter - space, p.y * distanceBetweenCenter, 1, space);
        }
        if (p.east != null) {
            g.fillRect(p.x * distanceBetweenCenter, p.y * distanceBetweenCenter + sizeOfPoint, space, 1);
        }
        if (p.south != null) {
            g.fillRect(p.x * distanceBetweenCenter + sizeOfPoint, p.y * distanceBetweenCenter, 1, space);
        }
        if (p.west != null) {
            g.fillRect(p.x * distanceBetweenCenter, p.y * distanceBetweenCenter - space, space, 1);
        }*/
    }

    private int midX(Point p) {
        return p.x * distanceBetweenCenter + sizeOfPoint / 2;
    }

    private int midY(Point p) {
        return p.y * distanceBetweenCenter + sizeOfPoint / 2;
    }
}

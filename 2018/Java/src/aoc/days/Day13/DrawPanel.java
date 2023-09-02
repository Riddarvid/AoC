package aoc.days.Day13;

import javax.swing.*;
import java.awt.*;

public class DrawPanel extends JPanel {
    private final Day13 model;
    private final int scale;

    public DrawPanel(Day13 model, int size) {
        scale = size/150;
        this.model = model;
        this.setDoubleBuffered(true);
        this.setPreferredSize(new Dimension(size, size));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.setColor(Color.BLACK);
        for (int y = 0; y < model.map.length; y++) {
            for (int x = 0; x < model.map[y].length; x++) {
                if (model.map[y][x] == '-') {
                    g.drawLine(x * scale, y * scale + scale / 2, x * scale + scale, y * scale + scale / 2);
                } else if (model.map[y][x] == '|') {
                    g.drawLine(x * scale + scale / 2, y * scale, x * scale + scale / 2, y * scale + scale);
                } else if (model.map[y][x] == '+') {
                    g.drawLine(x * scale, y * scale + scale / 2, x * scale + scale, y * scale + scale / 2);
                    g.drawLine(x * scale + scale / 2, y * scale, x * scale + scale / 2, y * scale + scale);
                }
                /*else if (model.map[y][x] == '/') {
                    g.drawLine(x * scale, y * scale + scale, x * scale + scale, y * scale);
                }*/
            }
        }
        g.setColor(Color.RED);
        for (Cart c : model.carts) {
            g.fillRect(c.getX() * scale, c.getY() * scale, scale, scale);
        }
    }
}

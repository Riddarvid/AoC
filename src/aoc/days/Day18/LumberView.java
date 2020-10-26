package aoc.days.Day18;

import javax.swing.*;
import java.awt.*;

public class LumberView extends JFrame {
    Day18 model;
    int size;
    int scale;

    public LumberView(Day18 model, int scale) {
        this.model = model;
        this.scale = scale;
        this.size = model.map.length * scale;
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(0, 0, size, size);
        setVisible(true);
        repaint();
    }

    public void update() {
        repaint();
    }

    @Override
    public void paint(Graphics g) {
        for (int row = 0; row < model.map.length; row++) {
            for (int col = 0; col < model.map[row].length; col++) {
                if (model.map[row][col] == '#') {
                    g.setColor(Color.ORANGE);
                } else if (model.map[row][col] == '.') {
                    g.setColor(Color.WHITE);
                } else {
                    g.setColor(Color.GREEN);
                }
                g.fillRect(col * scale, row * scale, scale,scale);
            }
        }
    }
}

package aoc.days.Day23;

import javax.swing.*;
import java.awt.*;

public class BotView extends JFrame {
    Day23 model;
    int scale = 10000;
    int width = 1000;
    int height = 1000;

    public BotView(Day23 model) {
        this.model = model;
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(30, 30, width, height);
        setBackground(Color.WHITE);
        setVisible(true);
        repaint();
    }

    public void update() {
        repaint();
    }

    @Override
    public void paint(Graphics g) {
        g.setColor(Color.RED);
        g.fillRect(width / 2, height / 2, 1, 1);
        g.setColor(Color.BLACK);
        for (NanoBot bot : model.nanoBots) {
            //g.fillRect();
        }
    }
}

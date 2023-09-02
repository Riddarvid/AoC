package aoc.days.Day13;

import javax.swing.*;

public class CartView extends JFrame {
    Day13 model;
    DrawPanel drawPanel;

    public CartView(Day13 model, int size) {
        this.model = model;
        this.drawPanel = new DrawPanel(model, size);
        this.add(drawPanel);
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(30, 30, size, size);
        setVisible(true);
    }

    public void update() {
        repaint();
    }
}

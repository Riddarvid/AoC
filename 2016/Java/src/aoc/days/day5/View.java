package aoc.days.day5;

import java.awt.*;

public class View {
    private final Canvas canvas;

    public View(StringBuilder password) {
        int width = 640;
        int height = 480;
        Frame f = new Frame("PasswordBreaker2000");
        canvas = new PasswordCanvas(password, width, height);
        f.add(canvas);
        f.setLayout(null);
        f.setSize(width, height);
        f.setVisible(true);
    }

    public void update() {
        canvas.repaint();
    }
}

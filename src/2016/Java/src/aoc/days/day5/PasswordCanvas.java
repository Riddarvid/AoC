package aoc.days.day5;

import java.awt.*;
import java.util.Random;

public class PasswordCanvas extends Canvas {
    private final StringBuilder password;
    private final int width;
    private final int height;
    private final Random random;

    public PasswordCanvas(StringBuilder password, int width, int height) {
        this.password = password;
        setBackground(Color.BLACK);
        setSize(width, height);
        this.width = width;
        this.height = height;
        random = new Random();
    }

    @Override
    public void paint(Graphics g) {
        StringBuilder randomString = new StringBuilder();
        for (int i = 0; i < 8 - password.length(); i++) {
            int value = random.nextInt(16);
            if (value < 10) {
                randomString.append(value);
            } else {
                randomString.append((char)('a' + (value - 10)));
            }
        }
        g.clearRect(0, 0, width, height);
        g.setColor(Color.GREEN);
        g.drawString(password.toString(), 50, 50);
        g.setColor(Color.RED);
        g.drawString(randomString.toString(), 50 + password.length() * 10, 50);
    }
}

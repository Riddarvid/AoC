package aoc15.days.day18;

import java.awt.*;

public class GridCanvas extends Canvas {
    private boolean[][] grid;

    public GridCanvas() {
        setBackground(Color.BLACK);
        setSize(1050, 1050);
    }

    public void update(boolean[][] grid) {
        this.grid = grid;
        repaint();
    }

    @Override
    public void paint(Graphics g) {
        g.setColor(Color.RED);
        g.drawRect(40, 40, 1000, 1000);
        g.setColor(Color.YELLOW);
        for (int row = 0; row < 100; row++) {
            for (int col = 0; col < 100; col++) {
                if (grid[row][col]) {
                    g.fillRect(40 + row * 10, 40 + col * 10, 10, 10);
                }
            }
        }
    }
}

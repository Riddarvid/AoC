package aoc.days.day13.view;

import aoc.days.day13.Tile;
import aoc.utils.geometry.Point2D;

import java.awt.*;
import java.util.Map;

public class View {
    private GameCanvas canvas;

    public View(Map<Point2D, Tile> objects) {
        Frame f = new Frame("GameView");
        canvas = new GameCanvas(objects);
        f.add(canvas);
        f.setLayout(null);
        f.setSize(1000, 600);
        f.setVisible(true);
    }

    public void update() {
        canvas.repaint();
    }
}

package aoc.days.day20.view;

import aoc.days.day20.Location;
import aoc.utils.geometry.Point2D;

import java.awt.*;
import java.util.HashSet;
import java.util.Set;

public class View {
    GameCanvas gc;

    public View(Set<Location> explored, Set<Location> foundLastCycle, Set<Point2D> points, int width, int height) {
        Frame f = new Frame("GameView");
        int side = 10;
        gc = new GameCanvas(explored, foundLastCycle, points, width, height, side);
        f.add(gc);
        f.setLayout(null);
        f.setSize(width * side + side, height * side + side);
        f.setVisible(true);
    }

    public void update(Set<Location> explored, Set<Location> foundLastCycle, Set<Point2D> point2DS) {
        gc.setExplored(new HashSet<>(explored));
        gc.setFoundLastCycle(new HashSet<>(foundLastCycle));
        gc.setPoints(new HashSet<>(point2DS));
        gc.repaint();
    }
}

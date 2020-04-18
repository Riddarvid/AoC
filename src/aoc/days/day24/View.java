package aoc.days.day24;

import aoc.utils.geometry.Point2D;

import java.awt.*;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class View {
    private MapCanvas mapCanvas;

    public View(int xSize, int ySize, Map<Point2D, Integer> pointToKey) {
        Frame f = new Frame("Map");
        mapCanvas = new MapCanvas(xSize, ySize, pointToKey);
        f.add(mapCanvas);
        f.setLayout(null);
        f.setSize(xSize, ySize);
        f.setVisible(true);
    }

    public void update(Map<Point2D, List<Edge>> edges) {
        mapCanvas.updateEdges(edges);
    }
}

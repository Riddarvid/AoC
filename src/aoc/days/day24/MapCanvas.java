package aoc.days.day24;

import aoc.utils.geometry.Point2D;

import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MapCanvas extends Canvas {
    private Map<Point2D, List<Edge>> edges;
    private Map<Point2D, Integer> pointToKey;
    private final int xSize;
    private final int ySize;

    public synchronized void updateEdges(Map<Point2D, List<Edge>> edges) {
        Map<Point2D, List<Edge>> copy = new HashMap<>();
        for (Point2D point : edges.keySet()) {
            List<Edge> edgeCopy = new ArrayList<>(edges.get(point));
            copy.put(point, edgeCopy);
        }
        this.edges = copy;
        repaint();
    }

    public MapCanvas(int xSize, int ySize, Map<Point2D, Integer> pointToKey) {
        this.pointToKey = pointToKey;
        this.xSize = xSize;
        this.ySize = ySize;
        setBackground(Color.WHITE);
        setSize(xSize, ySize);
    }

    public synchronized void paint(Graphics g) {
        int xOffset = 50;
        int yOffset = 300;
        int scale = 7;
        g.clearRect(0, 0, xSize, ySize);
        if (edges != null) {
            g.setColor(Color.BLACK);
            for (Point2D point : edges.keySet()) {
                for (Edge edge : edges.get(point)) {
                    Point2D other = edge.getEnd();
                    g.drawLine((int)point.getX() * scale + xOffset, (int)point.getY() * scale + yOffset, (int)other.getX() * scale + xOffset, (int)other.getY() * scale + yOffset);
                }
            }
            g.setColor(Color.RED);
            for (Point2D point : edges.keySet()) {
                if (pointToKey.containsKey(point)) {
                    g.drawString("" + pointToKey.get(point), (int)point.getX() * scale + xOffset, (int)point.getY() * scale + yOffset);
                }
            }
        }
    }
}

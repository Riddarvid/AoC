package aoc.days.day13.view;

import aoc.days.day13.Layer;
import aoc.days.day13.Packet;
import aoc.days.day13.Scanner;

import java.awt.*;
import java.util.List;
import java.util.Map;

public class ScannerCanvas extends Canvas {
    private Map<Integer, Layer> layerMap;
    private List<Packet> packets;
    private final int deepestLayer;
    private final int side = 10;

    public void update(Map<Integer, Layer> layerMap, List<Packet> packets) {
        this.layerMap = layerMap;
        this.packets = packets;
    }

    public ScannerCanvas(int deepestLayer) {
        this.deepestLayer = deepestLayer;
        setBackground(Color.WHITE);
        setSize(640, 480);
    }

    @Override
    public void paint(Graphics g) {
        g.clearRect(0, 0, 640, 480);
        for (int i = 0; i < deepestLayer; i++) {
            Layer current = layerMap.get(i);
            g.setColor(Color.BLACK);
            for (int range = 0; range <= current.getRange(); range++) {
                g.fillRect(i * side, range * side, side, side);
            }
            g.setColor(Color.BLUE);
            for (Layer layer : layerMap.values()) {
                int x = layer.getDepth() * side;
                int y = layer.getScannerDepth() * side;
                g.fillRect(x, y, side, side);
            }
            g.setColor(Color.GREEN);
            for (Packet packet : packets) {
                int x = packet.getPosition() * side;
                g.fillRect(x, 0, side, side);
            }
        }
    }
}

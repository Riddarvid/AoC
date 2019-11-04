package aoc.days.day13.view;

import aoc.days.day13.Layer;
import aoc.days.day13.Packet;

import java.awt.*;
import java.util.List;
import java.util.Map;

public class View {
    private ScannerCanvas canvas;

    public View(int deepestLayer) {
        Frame f = new Frame("ScannerView");
        canvas = new ScannerCanvas(deepestLayer);
        f.add(canvas);
        f.setLayout(null);
        f.setSize(640, 480);
        f.setVisible(true);
    }

    public void update(Map<Integer, Layer> layerMap, List<Packet> packets) {
        canvas.update(layerMap, packets);
    }
}

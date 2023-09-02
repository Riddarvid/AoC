package aoc.days.day22.view;

import aoc.days.day22.Node;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.List;

public class View {
    private InfectionCanvas canvas;

    public View(java.util.List<Node> infected, java.util.List<Node> weakened, List<Node> flagged) {
        Frame f = new Frame("Infection simulation");
        canvas = new InfectionCanvas(infected, weakened, flagged);
        f.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                super.windowClosing(e);
                f.dispose();
                System.exit(-1);
            }
        });
        f.add(canvas);
        f.setLayout(null);
        f.setSize(640, 480);
        f.setVisible(true);
    }

    public void update(int x, int y) {
        canvas.updateCoords(x, y);
        canvas.repaint();
    }
}

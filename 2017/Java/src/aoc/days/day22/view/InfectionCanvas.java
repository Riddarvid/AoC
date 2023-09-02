package aoc.days.day22.view;

import aoc.days.day22.Node;

import java.awt.*;
import java.util.List;

public class InfectionCanvas extends Canvas {
    private List<Node> infected;
    private List<Node> weakened;
    private List<Node> flagged;
    private int x;
    private int y;

    public void updateCoords(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public InfectionCanvas(List<Node> infected, List<Node> weakened, List<Node> flagged) {
        this.infected = infected;
        this.weakened = weakened;
        this.flagged = flagged;
        setBackground(Color.WHITE);
        setSize(640, 480);
    }

    @Override
    public void paint(Graphics g) {
        g.clearRect(0, 0, 640, 480);
        g.setColor(Color.RED);
        paintNodes(infected, g);
        g.setColor(Color.BLUE);
        paintNodes(weakened, g);
        g.setColor(Color.GREEN);
        paintNodes(flagged, g);
        g.setColor(Color.BLACK);
        g.fillRect((x + 40) * 9, (y + 30) * 9, 9, 9);
    }

    private void paintNodes(List<Node> nodes, Graphics g) {
        int size = 9;
        for (int i = 0; i < nodes.size(); i++) {
            Node n = nodes.get(i);
            g.fillRect((n.getX() + 40) * size, (n.getY() + 30) * size, size, size);
        }
    }
}

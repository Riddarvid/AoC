package AoC.Day15.FightView;

import AoC.Day15.FightModel.FightModel;
import AoC.Day15.FightModel.Point;
import AoC.Day15.FightModel.Unit;

import javax.swing.*;
import java.awt.*;

public class MapPanel extends JPanel {
    private FightModel model;
    private final int scale;

    public MapPanel(FightModel model, int size) {
        scale = size/32;
        this.model = model;
        setBackground(Color.BLACK);
        this.setDoubleBuffered(true);
        this.setPreferredSize(new Dimension(size, size));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.setColor(Color.WHITE);
        for (Point p : model.getMap()) {
            g.fillRect(p.getX() * scale, p.getY() * scale, scale, scale);
        }
        for (Unit u : model.getUnits()) {
            g.setColor(u.getTeam());
            g.fillRect(u.getX() * scale, u.getY() * scale, scale, scale);
            g.setColor(Color.BLACK);
            g.drawString(Integer.toString(u.getHealth()), u.getX() * scale, u.getY() * scale + scale / 2);
        }
    }

    public void reset(FightModel model) {
        this.model = model;
    }
}

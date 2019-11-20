package AoC.Day15.FightView;

import AoC.Day15.FightModel.FightModel;

import javax.swing.*;

public class FightView extends JFrame{
    FightModel model;
    MapPanel mapPanel;

    public FightView(FightModel model, int size) {
        this.model = model;
        this.mapPanel = new MapPanel(model, size);
        this.add(mapPanel);
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(30, 30, size, size);
        setVisible(true);
    }

    public void update() {
        repaint();
    }

    public void setModel(FightModel model) {
        this.model = model;
        mapPanel.reset(model);
    }
}

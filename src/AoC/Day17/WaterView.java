package AoC.Day17;

import javax.swing.*;
import java.awt.*;

public class WaterView extends JFrame {
    Day17 model;
    int xSize;
    int ySize;

    public WaterView(Day17 model, int xSize, int ySize) {
        this.xSize = xSize;
        this.ySize = ySize;
        this.model = model;
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(0, 0, xSize, ySize);
        setVisible(true);
    }

    public void update() {
        repaint();
    }

    @Override
    public void paint(Graphics g) {
        for (int row = 0; row < model.map.length; row++) {
            for (int col = 0; col < model.map[row].length; col++) {
                if (model.map[row][col] == '#') {
                    g.setColor(Color.BLACK);
                } else if (model.map[row][col] == '.') {
                    g.setColor(Color.WHITE);
                } else if (model.map[row][col] == '|'){
                    g.setColor(Color.RED);
                } else {
                    g.setColor(Color.BLUE);
                }
                g.fillRect(col, row, 1,1);
            }
        }
    }
}

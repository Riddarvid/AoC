package aoc.days.Day22;

import javax.swing.*;
import java.awt.*;

public class RegionView extends JFrame {
    Day22 model;
    int sizeOfPointX = 1;
    int sizeOfPointY = 1;
    int ySize = 1000;
    int xSize = 1000;

    public RegionView(Day22 model) {
        this.model = model;
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setBounds(30, 30, xSize, ySize);
        setBackground(Color.WHITE);
        setVisible(true);
        repaint();
    }

    public void update() {
        repaint();
    }

    @Override
    public void paint(Graphics g) {
        if (model.paintMap) {
            for (int row = 0; row < ySize / sizeOfPointY; row++) {
                for (int col = 0; col < xSize / sizeOfPointX; col++) {
                    Region r = model.regions[row][col];
                    if (r.getRiskLevel() == 0) {//ROCKY
                        g.setColor(Color.GREEN);
                    } else if (r.getRiskLevel() == 1) {//WET
                        g.setColor(Color.CYAN);
                    } else {//NARROW
                        g.setColor(Color.YELLOW);
                    }
                    g.fillRect(r.getX() * sizeOfPointX + 50, r.getY() * sizeOfPointY + 50, sizeOfPointX, sizeOfPointY);
                }
            }
        }
        if (model.paintPath) {
            Region r = model.target;
            g.setColor(Color.RED);
            while (r.getPrevious() != null) {
                g.fillRect(r.getX() * sizeOfPointX + 50, r.getY() * sizeOfPointY + 50, sizeOfPointX, sizeOfPointY);
                r = r.getPrevious();
            }
            g.fillRect(r.getX() * sizeOfPointX + 50, r.getY() * sizeOfPointY + 50, sizeOfPointX, sizeOfPointY);
        }
        if (model.paintDistance) {
            g.setColor(Color.BLACK);
            g.setFont(new Font(null, Font.PLAIN, 10));
            for (int row = 0; row < ySize / sizeOfPointY; row++) {
                for (int col = 0; col < xSize / sizeOfPointX; col++) {
                    Region r = model.regions[row][col];
                    Region o = model.regionsOtherTool[row][col];
                    /*if (o.getDistance() < r.getDistance()) {
                        r = o;
                    }*/
                    g.drawString(r.getDistance() + "/" + o.getDistance(), r.getX() * sizeOfPointX + 50, r.getY() * sizeOfPointY + 65);
                }
            }
        }
    }
}

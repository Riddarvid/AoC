package AoC.Day10;

import javax.swing.*;
import java.awt.*;

public class View extends JFrame implements Observer {
    private int minX;
    private int minY;
    private int width;
    private int height;
    private Model model;
    protected DrawPanel drawPanel;

    public View(String frameName, Model model, int width, int height) {
        this.width = width;
        this.height = height;
        this.minX = model.getMinX();
        this.minY = model.getMinY();
        this.model = model;
        initComponents(frameName);
        repaint();
    }

    private void initComponents(String frameName) {
        this.setTitle(frameName);
        this.setPreferredSize(new Dimension(width, height));
        this.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));

        drawPanel = new DrawPanel(model, width, height);
        this.add(drawPanel);

        // Make the frame pack all it's components by respecting the sizes if possible.
        this.pack();

        // Get the computer screen resolution
        Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
        // Center the frame
        this.setLocation(dim.width/2-this.getSize().width/2, dim.height/2-this.getSize().height/2);
        // Make the frame visible
        this.setVisible(true);
        // Make sure the frame exits when "x" is pressed
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    @Override
    public void act() {
        repaint();
    }
}

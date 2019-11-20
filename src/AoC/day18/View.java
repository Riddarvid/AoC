package AoC.day18;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class View extends Frame {
    private GridCanvas canvas;

    public View(boolean[][] grid) {
        addWindowListener(new WindowAdapter(){
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        canvas = new GridCanvas();
        add(canvas);
        setSize(1050, 1050);
        setLayout(null);
        setVisible(true);
        canvas.update(grid);
    }

    public void update(boolean[][] grid) {
        canvas.update(grid);
    }
}

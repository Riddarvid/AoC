package aoc.days.Day10;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Controller extends JFrame implements ActionListener {
    private Model model;
    private View view;
    private long delay = 5;
    private boolean forwards = true;

    public Controller(Model model, View view) {
        this.model = model;
        this.view = view;

        JPanel panel = new JPanel();
        JButton incSpeedButton = new JButton("Inc Speed");
        JButton decSpeedButton = new JButton("Dec Speed");
        JButton changeDirectionButton = new JButton("Change Direction");
        JButton pauseButton = new JButton("Pause");
        JButton stepForwardsButton = new JButton("Step Forward");
        JButton stepBackwardsButton = new JButton("Step Backwards");

        panel.add(incSpeedButton);
        panel.add(decSpeedButton);
        panel.add(changeDirectionButton);
        panel.add(pauseButton);
        panel.add(stepForwardsButton);
        panel.add(stepBackwardsButton);

        incSpeedButton.addActionListener(this);
        decSpeedButton.addActionListener(this);
        changeDirectionButton.addActionListener(this);
        pauseButton.addActionListener(this);
        stepForwardsButton.addActionListener(this);
        stepBackwardsButton.addActionListener(this);

        add(panel);
        pack();
        setVisible(true);

        try {
            while (true) {
                Thread.sleep(delay);
                if (forwards) {
                    model.updateForwards();
                } else {
                    model.updateBackwards();
                }
                view.repaint();
            }
        } catch (InterruptedException e) {

        }
    }


    @Override
    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();

        if (cmd.equals("Inc Speed")) {
            if (delay > 1) {
                delay--;
            }
        } else if (cmd.equals("Dec Speed")) {
            delay++;
        } else if (cmd.equals("Change Direction")) {
            model.changeDirection();
        } else if (cmd.equals("Pause")) {
            model.pause();
        } else if (cmd.equals("Step Forward")) {
            model.updateForwards();
            view.repaint();
        } else if (cmd.equals("Step Backwards")) {
            model.updateBackwards();
            view.repaint();
        }
    }
}

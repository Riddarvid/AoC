package aoc.days.Day15.FightController;

import aoc.days.Day15.FightModel.FightModel;
import aoc.days.Day15.FightModel.Unit;
import aoc.days.Day15.FightView.FightView;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class FightController {
    FightModel model;
    FightView view;
    int count = 0;

    public FightController(FightModel model, FightView view) {
        this.model = model;
        this.view = view;
        view.update();
        view.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                step();
            }

            @Override
            public void mousePressed(MouseEvent e) {

            }

            @Override
            public void mouseReleased(MouseEvent e) {

            }

            @Override
            public void mouseEntered(MouseEvent e) {

            }

            @Override
            public void mouseExited(MouseEvent e) {

            }
        });
    }

    private void step() {
        model.update();
        view.update();
        /*if (shouldEnd()) {
            if (Unit.isCombatEndedEarly()) {
                count--;
            }
            System.out.println("Total HP: " + model.getTotalHP() + "Number of completed turns: " + count);
        }*/
    }

    private boolean shouldEnd() {
        if (model.getUnits().isEmpty()) {
            return false;
        }
        Unit u = model.getUnits().get(0);
        for (Unit o : model.getUnits()) {
            if (u.getTeam() != o.getTeam()) {
                return false;
            }
        }
        return true;
    }

    public void run() {
        while (!shouldEnd()) {
            count++;
            step();
            /*try {
                Thread.sleep(5);
            } catch (InterruptedException e) {

            }*/
        }
        if (Unit.isCombatEndedEarly()) {
            count--;
        }
        System.out.println("Total HP: " + model.getTotalHP() + " Number of completed turns: " + count);
        System.out.println(model.getTotalHP() * count);
        if (Unit.isCombatEndedEarly()) {
            System.out.println("Combat ended early");
        }
        Unit.setCombatEndedEarly(false);
    }

    public void reset(FightModel model) {
        this.model = model;
        count = 0;
    }
}

package AoC.Day15;

import AoC.Day15.FightController.FightController;
import AoC.Day15.FightModel.FightModel;
import AoC.Day15.FightView.FightView;

import java.awt.*;

public class FightApplication {

    public static void main(String[] args) {
        int size = 960;
        int atkPwr = 3;
        FightModel model = new FightModel(atkPwr);
        FightView view = new FightView(model, size);
        FightController controller = new FightController(model, view);
        controller.run();
        while (!done(model)) {
            atkPwr++;
            model = new FightModel(atkPwr);
            view.setModel(model);
            controller.reset(model);
            controller.run();
        }
        System.out.println(atkPwr);
    }

    private static boolean done(FightModel model) {
        if (model.getUnits().get(0).getTeam() == Color.GREEN) {
            return false;
        }
        return model.getUnits().size() == model.getNumberOfElves();
    }
}

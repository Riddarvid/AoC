package AoC.Day15.Logger;

import AoC.Day15.FightModel.Unit;

public interface Observer {
    void actOnAttack(Unit attacker, Unit defender);
    void actOnMove(Unit actor);
}

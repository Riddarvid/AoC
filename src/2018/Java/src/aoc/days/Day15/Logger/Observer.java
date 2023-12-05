package aoc.days.Day15.Logger;

import aoc.days.Day15.FightModel.Unit;

public interface Observer {
    void actOnAttack(Unit attacker, Unit defender);
    void actOnMove(Unit actor);
}

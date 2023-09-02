package aoc.days.Day15.Logger;

import aoc.days.Day15.FightModel.Unit;

public class Logger implements Observer {

    @Override
    public void actOnAttack(Unit attacker, Unit defender) {
        System.out.println(attacker + " hit " + defender + " for " + attacker.getDamage() + " damage! It has " + defender.getHealth() + " health left!");
    }

    @Override
    public void actOnMove(Unit actor) {
        System.out.println(actor + " moved to " + actor.getX() + "," + actor.getY());
    }
}

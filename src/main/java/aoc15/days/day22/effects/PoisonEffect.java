package aoc15.days.day22.effects;

import aoc15.days.day22.StateBuilder;

public class PoisonEffect extends Effect {
    public PoisonEffect() {
        setTimer(6);
        name = "Poison";
    }

    private PoisonEffect(int timer) {
        setTimer(timer);
        name = "Poison";
    }

    @Override
    protected void applyEffectSpecific(StateBuilder stateBuilder) {
        stateBuilder.damageBoss(3);
    }

    @Override
    public Effect copy() {
        return new PoisonEffect(getTimer());
    }
}

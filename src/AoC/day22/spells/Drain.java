package AoC.day22.spells;

import AoC.day22.StateBuilder;

public class Drain extends Spell {
    public Drain() {
        cost = 73;
    }

    @Override
    void applyEffectSpecific(StateBuilder stateBuilder) {
        stateBuilder.damageBoss(2);
        stateBuilder.healPlayer(2);
    }

    @Override
    protected boolean isCastableSpecific(StateBuilder stateBuilder) {
        return true;
    }
}

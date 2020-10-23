package AoC.day22.spells;

import AoC.day22.StateBuilder;

public class MagicMissile extends Spell {
    public MagicMissile() {
        cost = 53;
    }

    @Override
    void applyEffectSpecific(StateBuilder stateBuilder) {
        stateBuilder.damageBoss(4);
    }

    @Override
    protected boolean isCastableSpecific(StateBuilder stateBuilder) {
        return true;
    }
}

package aoc15.days.day22.spells;

import aoc15.days.day22.StateBuilder;
import aoc15.days.day22.effects.Effect;
import aoc15.days.day22.effects.ShieldEffect;

public class Shield extends Spell {
    public Shield() {
        cost = 113;
    }

    @Override
    void applyEffectSpecific(StateBuilder stateBuilder) {
        Effect effect = new ShieldEffect();
        stateBuilder.addEffect(effect);
    }

    @Override
    protected boolean isCastableSpecific(StateBuilder stateBuilder) {
        return !stateBuilder.isEffectActive(new ShieldEffect());
    }
}

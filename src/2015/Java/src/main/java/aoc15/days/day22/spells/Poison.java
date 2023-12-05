package aoc15.days.day22.spells;

import aoc15.days.day22.StateBuilder;
import aoc15.days.day22.effects.Effect;
import aoc15.days.day22.effects.PoisonEffect;

public class Poison extends Spell {
    public Poison() {
        cost = 173;
    }

    @Override
    void applyEffectSpecific(StateBuilder stateBuilder) {
        Effect effect = new PoisonEffect();
        stateBuilder.addEffect(effect);
    }

    @Override
    protected boolean isCastableSpecific(StateBuilder stateBuilder) {
        return !stateBuilder.isEffectActive(new PoisonEffect());
    }
}

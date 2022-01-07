package aoc15.days.day22.spells;

import aoc15.days.day22.StateBuilder;
import aoc15.days.day22.effects.Effect;
import aoc15.days.day22.effects.RechargeEffect;

public class Recharge extends Spell {
    public Recharge() {
        cost = 229;
    }

    @Override
    void applyEffectSpecific(StateBuilder stateBuilder) {
        Effect effect = new RechargeEffect();
        stateBuilder.addEffect(effect);
    }

    @Override
    protected boolean isCastableSpecific(StateBuilder stateBuilder) {
        return !stateBuilder.isEffectActive(new RechargeEffect());
    }
}

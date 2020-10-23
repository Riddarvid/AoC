package AoC.day22.spells;

import AoC.day22.StateBuilder;
import AoC.day22.effects.Effect;
import AoC.day22.effects.PoisonEffect;
import AoC.day22.effects.RechargeEffect;

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

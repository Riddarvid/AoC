package AoC.day22.spells;

import AoC.day22.StateBuilder;
import AoC.day22.effects.Effect;
import AoC.day22.effects.PoisonEffect;
import AoC.day22.effects.ShieldEffect;

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

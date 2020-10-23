package AoC.day22.effects;

import AoC.day22.StateBuilder;

public class Decay extends Effect {
    public Decay() {
        setTimer(10000);
        name = "Decay";
    }

    @Override
    protected void applyEffectSpecific(StateBuilder stateBuilder) {
        stateBuilder.damagePlayer(1);
    }

    @Override
    public Effect copy() {
        return new Decay();
    }
}

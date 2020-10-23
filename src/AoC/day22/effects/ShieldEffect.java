package AoC.day22.effects;

import AoC.day22.StateBuilder;

public class ShieldEffect extends Effect {
    public ShieldEffect() {
        setTimer(6);
        name = "Shield";
    }

    private ShieldEffect(int timer) {
        setTimer(timer);
        name = "Shield";
    }

    @Override
    public void applyEffectSpecific(StateBuilder stateBuilder) {
        stateBuilder.setArmor(7);
    }

    @Override
    public Effect copy() {
        return new ShieldEffect(getTimer());
    }
}

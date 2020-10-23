package AoC.day22.effects;

import AoC.day22.StateBuilder;

public class RechargeEffect extends Effect {
    public RechargeEffect() {
        setTimer(5);
        name = "Recharge";
    }

    private RechargeEffect(int timer) {
        setTimer(timer);
        name = "Recharge";
    }

    @Override
    protected void applyEffectSpecific(StateBuilder stateBuilder) {
        stateBuilder.rechargeMana(101);
    }

    @Override
    public Effect copy() {
        return new RechargeEffect(getTimer());
    }
}

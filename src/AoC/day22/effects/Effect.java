package AoC.day22.effects;

import AoC.day22.StateBuilder;

public abstract class Effect {
    private int timer;
    protected String name;

    public int getTimer() {
        return timer;
    }

    public void applyEffect(StateBuilder stateBuilder) {
        if (timer <= 0 || timer > 6) {
            System.out.print("");
        }
        timer--;
        applyEffectSpecific(stateBuilder);
    }

    protected abstract void applyEffectSpecific(StateBuilder stateBuilder);

    protected void setTimer(int timer) {
        this.timer = timer;
    }

    public String getName() {
        return name;
    }

    public abstract Effect copy();
}

package AoC.day22;

import AoC.day21.Unit;

public abstract class Effect {
    private int timer;
    private AoC.day21.Unit target;

    public Effect(int timer, Unit target) {
        this.timer = timer;
        this.target = target;
    }

    void apply() {
        if (!(timer <= 0)) {
            timer--;
            applySpecific();
        }
    }

    public int getTimer() {
        return timer;
    }

    protected abstract void applySpecific();
}

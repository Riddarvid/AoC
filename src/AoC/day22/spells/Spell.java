package AoC.day22.spells;

import AoC.day22.StateBuilder;

public abstract class Spell {
    protected int cost;

    public int getCost() {
        return cost;
    }

    public void applyEffect(StateBuilder stateBuilder) {
        stateBuilder.spendMana(cost);
        applyEffectSpecific(stateBuilder);
    }

    abstract void applyEffectSpecific(StateBuilder stateBuilder);

    public boolean isCastable(StateBuilder stateBuilder) {
        if (stateBuilder.getMana() < cost) {
            return false;
        } else {
            return isCastableSpecific(stateBuilder);
        }
    }

    protected abstract boolean isCastableSpecific(StateBuilder stateBuilder);
}

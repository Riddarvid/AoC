package AoC.day22;

import AoC.day22.actors.Boss;
import AoC.day22.actors.Player;
import AoC.day22.effects.Decay;
import AoC.day22.effects.Effect;
import AoC.day22.spells.*;

import java.util.ArrayList;
import java.util.List;

public class State {
    private final Player player;
    private final Boss boss;
    private final List<Effect> effects;
    private final int manaSpent;
    private final List<Spell> castedSpells;

    public State(Player player, Boss boss, List<Effect> effects, int manaSpent, List<Spell> castedSpells) {
        this.player = player;
        this.boss = boss;
        this.effects = effects;
        this.manaSpent = manaSpent;
        this.castedSpells = castedSpells;
    }

    public State(Player player, Boss boss, boolean part2) {
        this.player = player;
        this.boss = boss;
        this.effects = new ArrayList<>();
        this.manaSpent = 0;
        this.castedSpells = new ArrayList<>();
        if (part2) {
            effects.add(new Decay());
        }
    }

    public State generateState(Spell spell) {
        StateBuilder stateBuilder = new StateBuilder(this);
        stateBuilder.playersTurn(spell);
        stateBuilder.resetArmor();
        stateBuilder.bossTurn();
        stateBuilder.resetArmor();
        return stateBuilder.toState();
    }

    public Player getPlayer() {
        return player;
    }

    public Boss getBoss() {
        return boss;
    }

    public List<Effect> getEffects() {
        return new ArrayList<>(effects);
    }

    public int getManaSpent() {
        return manaSpent;
    }

    public boolean isFinished() {
        return player.isDead() || boss.isDead();
    }

    public boolean playerWon() {
        return boss.isDead();
    }

    public List<State> generateNextStates() {
        List<State> states = new ArrayList<>();
        State state = generateState(new MagicMissile());
        if (state.playerAlive()) {
            states.add(state);
        }
        state = generateState(new Drain());
        if (state.playerAlive()) {
            states.add(state);
        }
        state = generateState(new Poison());
        if (state.playerAlive()) {
            states.add(state);
        }
        state = generateState(new Shield());
        if (state.playerAlive()) {
            states.add(state);
        }
        state = generateState(new Recharge());
        if (state.playerAlive()) {
            states.add(state);
        }
        return states;
    }

    private boolean playerAlive() {
        return !player.isDead();
    }

    public List<Spell> getCastedSpells() {
        return new ArrayList<>(castedSpells);
    }
}

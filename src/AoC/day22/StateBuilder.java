package AoC.day22;

import AoC.day22.actors.Boss;
import AoC.day22.actors.Player;
import AoC.day22.effects.Effect;
import AoC.day22.spells.Poison;
import AoC.day22.spells.Spell;

import java.util.ArrayList;
import java.util.List;

public class StateBuilder {
    private Player player;
    private Boss boss;
    private List<Effect> effects;
    private List<Spell> castedSpells;
    private int manaSpent;
    private boolean playerDead = false;
    private boolean bossDead = false;

    public StateBuilder(State state) {
        player = state.getPlayer();
        boss = state.getBoss();
        effects = new ArrayList<>();
        for (Effect effect : state.getEffects()) {
            this.effects.add(effect.copy());
        }
        castedSpells = state.getCastedSpells();
        manaSpent = state.getManaSpent();
    }

    public void playersTurn(Spell spell) {
        if (shouldContinue()) {
            applyEffects();
        }
        if (shouldContinue()) {
            if (spell.isCastable(this)) {

                castedSpells.add(spell);
                spell.applyEffect(this);
            } else {
                player = player.takeDamage(100000);
                setPlayerDead();
            }
        }
    }

    public void bossTurn() {
        if (shouldContinue()) {
            applyEffects();
        }
        if (shouldContinue()) {
            damagePlayer();
        }
    }

    private void damagePlayer() {
        player = player.takeDamage(boss.getDamage());
        playerDead = player.isDead();
    }

    private void applyEffects() {
        for (Effect effect : effects) {
            effect.applyEffect(this);
        }
        removeFinishedEffects();
    }

    private void removeFinishedEffects() {
        List<Effect> toRemove = new ArrayList<>();
        for (Effect effect : effects) {
            if (effect.getTimer() == 0) {
                toRemove.add(effect);
            }
        }
        effects.removeAll(toRemove);
    }

    private boolean shouldContinue() {
        if (player.isDead()) {
            playerDead = true;
        }
        if (boss.isDead()) {
            bossDead = true;
        }
        return !(playerDead || bossDead);
    }

    public State toState() {
        return new State(player, boss, new ArrayList<>(effects), manaSpent, castedSpells);
    }

    public void setPlayerDead() {
        playerDead = true;
    }

    public Player getPlayer() {
        return player;
    }

    public Boss getBoss() {
        return boss;
    }

    public void spendMana(int cost) {
        player = player.spendMana(cost);
        manaSpent += cost;
    }

    public int getMana() {
        return player.getMana();
    }

    public void damageBoss(int damage) {
        boss = boss.takeDamage(damage);
    }

    public void healPlayer(int healing) {
        player = player.heal(healing);
    }

    public void setArmor(int armorIncrease) {
        player = player.setArmor(armorIncrease);
    }

    public void resetArmor() {
        player = player.setArmor(0);
    }

    public void addEffect(Effect effect) {
        effects.add(effect);
    }

    public void rechargeMana(int rechargedMana) {
        player = player.rechargeMana(rechargedMana);
    }

    public boolean isEffectActive(Effect effect) {
        for (Effect active : effects) {
            if (active.getName().equals(effect.getName())) {
                return true;
            }
        }
        return false;
    }

    public void damagePlayer(int damage) {
        player = player.takeDamage(damage);
    }
}

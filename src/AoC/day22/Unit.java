package AoC.day22;

import java.util.ArrayList;
import java.util.List;

public class Unit {
    private int hp;
    private int damage;
    private int armor;
    private List<Effect> effects;

    public Unit(int hp, int damage) {
        this.hp = hp;
        this.damage = damage;
        armor = 0;
        effects = new ArrayList<>();
    }

    public void takeDamage(int damage) {
        int hpLoss = damage - armor;
        if (hpLoss <= 0) {
            hpLoss = 1;
        }
        hp -= hpLoss;
    }

    public void heal(int healAmount) {
        hp += healAmount;
    }

    public void addEffect(Effect effect) {
        if (effects.contains(effect)) {

        }
    }
}

package AoC.day21;

import java.util.ArrayList;
import java.util.List;

public class Unit {
    private int hp;
    private final int baseDamage;
    private final int baseArmor;
    private final List<Equipment> equipment;

    public Unit(int hp, int baseDamage, int baseArmor) {
        this.hp = hp;
        this.baseDamage = baseDamage;
        this.baseArmor = baseArmor;
        equipment = new ArrayList<>();
    }

    void addEquipment(Equipment newEquipment) {
        equipment.add(newEquipment);
    }

    private int getDamage() {
        int sum = 0;
        for (Equipment e : equipment) {
            sum += e.getDamage();
        }
        sum += baseDamage;
        return sum;
    }

    private int getArmor() {
        int sum = 0;
        for (Equipment e : equipment) {
            sum += e.getArmor();
        }
        sum += baseArmor;
        return sum;
    }

    int getCost() {
        int sum = 0;
        for (Equipment e : equipment) {
            sum += e.getCost();
        }
        return sum;
    }

    void attack(Unit other) {
        if (!isDead()) {
            int damage = getDamage();
            other.takeDamage(damage);
        }
    }

    private void takeDamage(int damage) {
        int armor = getArmor();
        int hpLoss = damage - armor;
        if (hpLoss <= 0) {
            hpLoss = 1;
        }
        hp -= hpLoss;
    }

    boolean isDead() {
        return hp <= 0;
    }
}

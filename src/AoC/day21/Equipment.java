package AoC.day21;

import java.util.Objects;

public class Equipment {
    private final String name;
    private final int cost;
    private final int damage;
    private final int armor;

    public Equipment(String name, int cost, int damage, int armor) {
        this.name = name;
        this.cost = cost;
        this.damage = damage;
        this.armor = armor;
    }

    public int getCost() {
        return cost;
    }

    public int getDamage() {
        return damage;
    }

    public int getArmor() {
        return armor;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Equipment equipment = (Equipment) o;
        return cost == equipment.cost &&
                damage == equipment.damage &&
                armor == equipment.armor &&
                Objects.equals(name, equipment.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, cost, damage, armor);
    }
}

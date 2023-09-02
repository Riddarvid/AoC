package aoc15.days.day22.actors;

public class Player {
    private final int hp;
    private final int mana;
    private final int armor;

    public Player(int hp, int mana, int armor) {
        this.hp = hp;
        this.mana = mana;
        this.armor = armor;
    }


    public Player takeDamage(int damage) {
        int realDamage = damage - armor;
        if (realDamage <= 0) {
            realDamage = 1;
        }
        return new Player(hp - realDamage, mana, armor);
    }

    public boolean isDead() {
        return hp <= 0 || mana < 0;
    }

    public int getMana() {
        return mana;
    }

    public Player spendMana(int cost) {
        return new Player(hp, mana - cost, armor);
    }

    @Override
    public String toString() {
        return "- Player has " + hp + " hit points, " + armor + " armor, " + mana + " mana";
    }

    public Player heal(int healing) {
        return new Player(hp + healing, mana, armor);
    }

    public Player increaseArmor(int armorIncrease) {
        return new Player(hp, mana, armor + armorIncrease);
    }

    public Player setArmor(int armor) {
        return new Player(hp, mana, armor);
    }

    public Player rechargeMana(int rechargedMana) {
        return new Player(hp, mana + rechargedMana, armor);
    }

    public int getHP() {
        return hp;
    }
}

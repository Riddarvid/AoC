package aoc15.days.day22.actors;

public class Boss {
    private final int hp;
    private final int damage;

    public Boss(int hp, int damage) {
        this.hp = hp;
        this.damage = damage;
    }

    public int getDamage() {
        return damage;
    }

    public Boss takeDamage(int damageReceived) {
        return new Boss(hp - damageReceived, this.damage);
    }

    public boolean isDead() {
        return hp <= 0;
    }

    @Override
    public String toString() {
        return "- Boss has " + hp + " hit points";
    }

    public int getHP() {
        return hp;
    }
}

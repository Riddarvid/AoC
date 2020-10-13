package AoC.day21;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day21 extends Day {
    private final int bossHP = 100;
    private final int bossDamage = 8;
    private final int bossArmor = 2;
    private List<Equipment> weapons;
    private List<Equipment> armor;
    private List<Equipment> rings;

    public static void main(String[] args) {
        new Day21();
    }

    @Override
    protected void part1() {
        int lowestCost = Integer.MAX_VALUE;
        for (Equipment weapon : weapons) {
            for (Equipment armorPiece : armor) {
                for (Equipment ring1 : rings) {
                    for (Equipment ring2 : rings) {
                        if (ring1.equals(ring2)) {
                            continue;
                        }
                        Unit boss = new Unit(bossHP, bossDamage, bossArmor);
                        Unit player = new Unit(100, 0, 0);
                        player.addEquipment(weapon);
                        player.addEquipment(armorPiece);
                        player.addEquipment(ring1);
                        player.addEquipment(ring2);
                        while (!fightIsDone(boss, player)) {
                            player.attack(boss);
                            boss.attack(player);
                        }
                        if (!player.isDead()) {
                            int currentCost = player.getCost();
                            if (currentCost < lowestCost) {
                                lowestCost = currentCost;
                            }
                        }
                    }
                }
            }
        }
        System.out.println(lowestCost);
    }

    private boolean fightIsDone(Unit boss, Unit player) {
        return player.isDead() || boss.isDead();
    }

    @Override
    protected void part2() {
        int highestCost = 0;
        for (Equipment weapon : weapons) {
            for (Equipment armorPiece : armor) {
                for (Equipment ring1 : rings) {
                    for (Equipment ring2 : rings) {
                        if (ring1.equals(ring2)) {
                            continue;
                        }
                        Unit boss = new Unit(bossHP, bossDamage, bossArmor);
                        Unit player = new Unit(100, 0, 0);
                        player.addEquipment(weapon);
                        player.addEquipment(armorPiece);
                        player.addEquipment(ring1);
                        player.addEquipment(ring2);
                        while (!fightIsDone(boss, player)) {
                            player.attack(boss);
                            boss.attack(player);
                        }
                        if (player.isDead()) {
                            int currentCost = player.getCost();
                            if (currentCost > highestCost) {
                                highestCost = currentCost;
                            }
                        }
                    }
                }
            }
        }
        System.out.println(highestCost);
    }

    @Override
    protected void setup() {
        weapons = new ArrayList<>();
        weapons.add(new Equipment("Dagger", 8, 4, 0));
        weapons.add(new Equipment("Shortsword", 10, 5, 0));
        weapons.add(new Equipment("Warhammer", 25, 6, 0));
        weapons.add(new Equipment("Longsword", 40, 7, 0));
        weapons.add(new Equipment("Greataxe", 74, 8, 0));
        armor = new ArrayList<>();
        armor.add(new Equipment("None", 0, 0, 0));
        armor.add(new Equipment("Leather", 13, 0, 1));
        armor.add(new Equipment("None", 31, 0, 2));
        armor.add(new Equipment("None", 53, 0, 3));
        armor.add(new Equipment("None", 75, 0, 4));
        armor.add(new Equipment("None", 102, 0, 5));
        rings = new ArrayList<>();
        rings.add(new Equipment("none1", 0, 0, 0));
        rings.add(new Equipment("none2", 0, 0, 0));
        rings.add(new Equipment("Damage +1", 25, 1, 0));
        rings.add(new Equipment("Damage +2", 50, 2, 0));
        rings.add(new Equipment("Damage +3", 100, 3, 0));
        rings.add(new Equipment("Defense +1", 20, 0, 1));
        rings.add(new Equipment("Defense +2", 40, 0, 2));
        rings.add(new Equipment("Defense +3", 80, 0, 3));
    }
}

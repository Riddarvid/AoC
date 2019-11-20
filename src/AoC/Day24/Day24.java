package AoC.Day24;

import AoC.FileUtilities;
import AoC.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Day24 {
    private List<Group> immuneSystem;
    private List<Group> infection;
    private int boost = -1;
    static boolean debug = false;

    public static void main(String[] args) {
        new Day24().run();
    }

    private void run() {
        while (!hasWon(immuneSystem)) {
            boost++;
            String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day24/input.txt"));
            //immuneSystem = initGroups(input, 1, 2 , Faction.IMMUNE_SYSTEM);
            //infection = initGroups(input, 5, 6 , Faction.INFECTION);
            immuneSystem = initGroups(input, 1, 10 , Faction.IMMUNE_SYSTEM);
            infection = initGroups(input, 13, 22 , Faction.INFECTION);
            List<Group> previousImmuneSystem = copyList(immuneSystem);
            List<Group> previousInfection = copyList(infection);
            while (!isDone()) {
                fight();
                if (previousImmuneSystem.equals(immuneSystem) && previousInfection.equals(infection)) {
                    System.out.println("Stalemate");
                    break;
                }
                previousImmuneSystem = copyList(immuneSystem);
                previousInfection = copyList(infection);
            }
            printState();
            System.out.println("Winning force contains " + getNumberOfWinningUnits() + " units!");
            if (getNumberOfWinningUnits() == 4529) {
                System.out.println("hmmm");
            }
        }
        System.out.println("Done! Boost needed: " + boost);
    }

    private List<Group> copyList(List<Group> source) {
        List<Group> target = new ArrayList<>();
        for (Group g : source) {
            target.add(g.getCopy());
        }
        return target;
    }

    private boolean listsEqual(List<Group> list1, List<Group> list2) {
        if (list1.size() != list2.size()) {
            return false;
        }
        for (int i = 0; i < list1.size(); i++) {
            if (!list1.equals(list2)) {
                return false;
            }
        }
        return true;
    }

    private boolean hasWon(List<Group> side) {
        if (side != null) {
            return infection.isEmpty() && !side.isEmpty();
        }
        return false;
    }

    private int getNumberOfWinningUnits() {
        int sum = 0;
        if (immuneSystem.isEmpty()) {
            for (Group g : infection) {
                sum += g.getNumberOfUnits();
            }
        } else {
            for (Group g : immuneSystem) {
                sum += g.getNumberOfUnits();
            }
        }
        return sum;
    }

    private void fight() {
        if (debug) {
            printState();
        }
        target();
        attack();
        removeDead();
    }

    private void removeDead() {
        List<Group> toRemove = new ArrayList<>();
        for (Group g : immuneSystem) {
            if (g.getNumberOfUnits() <= 0) {
                toRemove.add(g);
            }
        }
        immuneSystem.removeAll(toRemove);
        toRemove = new ArrayList<>();
        for (Group g : infection) {
            if (g.getNumberOfUnits() <= 0) {
                toRemove.add(g);
            }
        }
        infection.removeAll(toRemove);
    }

    private void attack() {
        Queue<Group> pq = new PriorityQueue<>(new AttackingComp());
        pq.addAll(infection);
        pq.addAll(immuneSystem);
        while (!pq.isEmpty()) {
            pq.poll().attack();
        }
    }

    private void target() {
        Queue<Group> pq = new PriorityQueue<>(new TargetingComp());
        pq.addAll(infection);
        List<Group> potentialTargets = new ArrayList<>(immuneSystem);
        while (!pq.isEmpty() && !potentialTargets.isEmpty()) {
            pq.poll().target(potentialTargets);
        }
        pq = new PriorityQueue<>(new TargetingComp());
        pq.addAll(immuneSystem);
        potentialTargets = new ArrayList<>(infection);
        while (!pq.isEmpty() && !potentialTargets.isEmpty()) {
            pq.poll().target(potentialTargets);
        }
    }

    private boolean isDone() {
        return infection.isEmpty() || immuneSystem.isEmpty();
    }

    private List<Group> initGroups(String[] strings, int start, int end, Faction faction) {
        List<Group> groups = new ArrayList<>();
        for (int i = start; i <= end; i++) {
            String s = strings[i];
            List<Integer> integers = RegEx.getIntegers(s);
            int damage = integers.get(2);
            if (faction == Faction.IMMUNE_SYSTEM) {
                damage += boost;
            }
            List<DamageType> immunities = RegEx.getImmunities(s);
            List<DamageType> weaknesses = RegEx.getWeaknesses(s);
            DamageType damageType = RegEx.getDamageType(s);
            groups.add(new Group(faction, integers.get(0), integers.get(1), immunities, weaknesses, damage, damageType , integers.get(3), i - start + 1));
        }
        return groups;
    }

    void printState() {
        System.out.println();
        System.out.println("Immune System:");
        for (Group g : immuneSystem) {
            System.out.println("Group " + g.getId() + " contains " + g.getNumberOfUnits() + " units");
        }
        System.out.println("\nInfection:");
        for (Group g : infection) {
            System.out.println("Group " + g.getId() + " contains " + g.getNumberOfUnits() + " units");
        }
        System.out.println();
    }
}

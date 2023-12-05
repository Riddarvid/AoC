package aoc.days.day24;

import aoc.days.Day;
import aoc.utils.InputUtilities;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day24 extends Day {
    private Set<Component> components;

    public static void main(String[] args) {
        new Day24();
    }

    @Override
    protected void part1() {
        System.out.println(getBestStrength(components));
    }

    private int getBestStrength(Set<Component> components) {
        return getBestStrength(components, new Bridge());
    }

    private int getBestStrength(Set<Component> remaining, Bridge soFar) {
        Set<Component> validExtensions = soFar.getValidExtensions(remaining);
        int best = soFar.strength();
        for (Component extension : validExtensions) {
            Bridge extended = soFar.extend(extension);
            Set<Component> newRemaining = new HashSet<>(remaining);
            newRemaining.remove(extension);
            int score = getBestStrength(newRemaining, extended);
            if (score > best) {
                best = score;
            }
        }
        return best;
    }

    @Override
    protected void part2() {
        int maxLength = getMaxLength(components);
        Set<Bridge> bridges = getBridgesOfLength(maxLength, components);
        int best = 0;
        for (Bridge bridge : bridges) {
            if (bridge.strength() > best) {
                best = bridge.strength();
            }
        }
        System.out.println(best);
    }

    private Set<Bridge> getBridgesOfLength(int maxLength, Set<Component> components) {
        return getBridgesOfLength(maxLength, components, new Bridge());
    }

    private Set<Bridge> getBridgesOfLength(int maxLength, Set<Component> remaining, Bridge soFar) {
        if (soFar.length() == maxLength) {
            Set<Bridge> bridges = new HashSet<>();
            bridges.add(soFar);
            return bridges;
        }
        Set<Component> validExtensions = soFar.getValidExtensions(remaining);
        Set<Bridge> bridges = new HashSet<>();
        for (Component extension : validExtensions) {
            Bridge extended = soFar.extend(extension);
            Set<Component> newRemaining = new HashSet<>(remaining);
            newRemaining.remove(extension);
            bridges.addAll(getBridgesOfLength(maxLength, newRemaining, extended));
        }
        return bridges;
    }

    private int getMaxLength(Set<Component> components) {
        return getMaxLength(components, new Bridge());
    }

    private int getMaxLength(Set<Component> remaining, Bridge soFar) {
        Set<Component> validExtensions = soFar.getValidExtensions(remaining);
        int best = soFar.length();
        for (Component extension : validExtensions) {
            Bridge extended = soFar.extend(extension);
            Set<Component> newRemaining = new HashSet<>(remaining);
            newRemaining.remove(extension);
            int score = getMaxLength(newRemaining, extended);
            if (score > best) {
                best = score;
            }
        }
        return best;
    }

    @Override
    protected void setup() {
        components = new HashSet<>();
        for (String s : lines) {
            List<Integer> values = InputUtilities.getInts(s);
            Component c = new Component(values.get(0), values.get(1));
            components.add(c);
        }
    }
}

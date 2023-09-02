package days.day7;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.*;

public class Day7 extends Day {
    private Map<String, Bag> bags;

    public static void main(String[] args) {
        new Day7();
    }

    @Override
    public long part1() {
        return canContain(bags.get("shiny gold")).size();
    }

    private Set<Bag> canContain(Bag target) {
        Set<Bag> canContain = new HashSet<>();
        Set<Bag> discoveredLast = new HashSet<>();
        discoveredLast.add(target);
        while (!discoveredLast.isEmpty()) {
            Set<Bag> discoveredNow = new HashSet<>();
            for (Bag inner : discoveredLast) {
                for (Bag containing : bags.values()) {
                    if (!canContain.contains(containing) && containing.contains(inner)) {
                        canContain.add(containing);
                        discoveredNow.add(containing);
                    }
                }
            }
            discoveredLast = discoveredNow;
        }
        return canContain;
    }

    @Override
    public long part2() {
        Bag bag = bags.get("shiny gold");
        long numberOfBags = bag.getNumberOfBags();
        return numberOfBags;
    }

    @Override
    public void setup() {
        bags = new HashMap<>();
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            String name = tokens.get(0) + " " + tokens.get(1);
            Bag bag = new Bag(name);
            bags.put(name, bag);
        }
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            String name = tokens.get(0) + " " + tokens.get(1);
            Bag bag = bags.get(name);
            if (!s.endsWith("other bags.")) {
                int i = 4;
                while (i < tokens.size()) {
                    String contentName = tokens.get(i + 1) + " " + tokens.get(i + 2);
                    Bag inner = bags.get(contentName);
                    int amount = Integer.parseInt(tokens.get(i));
                    bag.addContents(inner, amount);
                    i += 4;
                }
            }
            bags.put(name, bag);
        }
    }
}

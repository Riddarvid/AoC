package aoc15.days.day19;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.*;

public class Day19 extends Day {
    private Molecule molecule;
    private List<Rule> rules;

    @Override
    public long part1() {
        Set<Molecule> unique = new HashSet<>();
        for (Rule rule : rules) {
            unique.addAll(rule.apply(molecule));
        }
        return unique.size();
    }

    @Override
    public long part2() {
        Molecule goal = new Molecule("e");
        Molecule current = molecule;
        int steps = 0;
        while (current != null && !current.equals(goal)) {
            Queue<Molecule> moleculeQueue = new PriorityQueue<>(new MoleculeComparator());
            steps++;
            moleculeQueue.addAll(applyRulesBackwards(current));
            current = moleculeQueue.poll();
        }
        if (current != null) {
            return steps;
        } else {
            throw new IllegalArgumentException("No solution found");
        }
    }

    private Set<Molecule> applyRulesBackwards(Molecule current) {
        Set<Molecule> replaced = new HashSet<>();
        for (Rule rule : rules) {
            replaced.addAll(rule.applyBackwards(current));
        }
        return replaced;
    }


    @Override
    public void setup() {
        molecule = new Molecule(lines.get(lines.size() - 1));
        lines.remove(lines.size() - 1);
        rules = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            rules.add(new Rule(tokens.get(0), tokens.get(2)));
        }
    }
}

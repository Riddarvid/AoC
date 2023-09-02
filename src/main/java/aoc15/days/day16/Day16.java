package aoc15.days.day16;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day16 extends Day {
    private List<Sue> sues;
    private Sue correct;

    @Override
    public long part1() {
        List<Sue> potential = new ArrayList<>();
        for (Sue other : sues) {
            if (correct.isPotential(other)) {
                potential.add(other);
            }
        }
        if (potential.size() != 1) {
            throw new IllegalArgumentException("Wrong number of Sues found.");
        }
        return potential.get(0).getId();
    }

    @Override
    public long part2() {
        List<Sue> potential = new ArrayList<>();
        for (Sue other : sues) {
            if (correct.isPotential2(other)) {
                potential.add(other);
            }
        }
        if (potential.size() != 1) {
            throw new IllegalArgumentException("Wrong number of Sues found.");
        }
        return potential.get(0).getId();
    }

    @Override
    public void setup() {
        List<Sample> correctSamples = new ArrayList<>();
        correctSamples.add(new Sample(Compound.CHILDREN, 3));
        correctSamples.add(new Sample(Compound.CATS, 7));
        correctSamples.add(new Sample(Compound.SAMOYEDS, 2));
        correctSamples.add(new Sample(Compound.POMERANIANS, 3));
        correctSamples.add(new Sample(Compound.AKITAS, 0));
        correctSamples.add(new Sample(Compound.VIZSLAS, 0));
        correctSamples.add(new Sample(Compound.GOLDFISH, 5));
        correctSamples.add(new Sample(Compound.TREES, 3));
        correctSamples.add(new Sample(Compound.CARS, 2));
        correctSamples.add(new Sample(Compound.PERFUMES, 1));
        correct = new Sue(correctSamples);
        sues = new ArrayList<>();
        for (int i = 1; i < lines.size(); i++) {
            String s = lines.get(i);
            sues.add(generateSue(s));
        }
    }

    private Sue generateSue(String s) {
        s = s.replace(",", "");
        s = s.replace(":", "");
        List<String> tokens = ParsingUtils.getTokens(s, ' ');
        Sue sue = new Sue(Integer.parseInt(tokens.get(1)));
        for (int i = 2; i < tokens.size(); i += 2) {
            sue.addSample(new Sample(tokens.get(i), Integer.parseInt(tokens.get(i + 1))));
        }
        return sue;
    }
}

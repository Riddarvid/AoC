package AoC.day16;

import AoC.FileUtilities;
import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day16 extends Day {
    private List<Sue> sues;
    private Sue correct;

    public static void main(String[] args) {
        new Day16();
    }

    @Override
    protected void part1() {
        List<Sue> potential = new ArrayList<>();
        for (Sue other : sues) {
            if (correct.isPotential(other)) {
                potential.add(other);
            }
        }
        for (Sue sue : potential) {
            System.out.println(sue.getId());
        }
    }

    @Override
    protected void part2() {
        List<Sue> potential = new ArrayList<>();
        for (Sue other : sues) {
            if (correct.isPotential2(other)) {
                potential.add(other);
            }
        }
        for (Sue sue : potential) {
            System.out.println(sue.getId());
        }
    }

    @Override
    protected void setup() {
        sues = new ArrayList<>();
        correct = generateSue(lines.get(0));
        for (int i = 1; i < lines.size(); i++) {
            String s = lines.get(i);
            sues.add(generateSue(s));
        }
    }

    private Sue generateSue(String s) {
        s = s.replace(',', ' ');
        s = s.replace(':', ' ');
        List<String> tokens = FileUtilities.getTokens(s, ' ');
        Sue sue = new Sue(Integer.parseInt(tokens.get(1)));
        for (int i = 2; i < tokens.size(); i += 2) {
            sue.addSample(new Sample(tokens.get(i), Integer.parseInt(tokens.get(i + 1))));
        }
        return sue;
    }
}

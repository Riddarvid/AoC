package AoC.day2;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day2 extends Day {
    private List<Present> presents;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    protected void part1() {
        int totalArea = 0;
        for (Present present : presents) {
            totalArea += present.paperRequired;
        }
        System.out.println(totalArea);
    }

    @Override
    protected void part2() {
        int totalLength = 0;
        for (Present present : presents) {
            totalLength += present.ribbonRequired;
        }
        System.out.println(totalLength);
    }

    @Override
    protected void setup() {
        presents = new ArrayList<>();
        for (String s : lines) {
            presents.add(new Present(ParsingUtils.getIntegers(s)));
        }
    }


}

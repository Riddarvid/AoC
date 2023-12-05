package aoc15.days.day2;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day2 extends Day {
    private List<Present> presents;

    @Override
    public long part1() {
        int totalArea = 0;
        for (Present present : presents) {
            totalArea += present.paperRequired;
        }
        return totalArea;
    }

    @Override
    public long part2() {
        int totalLength = 0;
        for (Present present : presents) {
            totalLength += present.ribbonRequired;
        }
        return totalLength;
    }

    @Override
    public void setup() {
        presents = new ArrayList<>();
        for (String s : lines) {
            presents.add(new Present(ParsingUtils.getIntegers(s)));
        }
    }


}

package AoC.day2;

import AoC.Day;
import AoC.FileUtilities;

import java.util.ArrayList;
import java.util.List;

public class Day2 extends Day {
    private List<Present> presents = new ArrayList<>();

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
        for (String s : lines) {
            presents.add(new Present(FileUtilities.getInts(s)));
        }
    }


}

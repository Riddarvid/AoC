package aoc.days.Day12;

import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;

public class Day12 {
    public static void main(String[] args) {
        new Day12();
    }

    public Day12() {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day12/input.txt"));
        boolean[] rules = RegEx.getRules(input);
        boolean[] initialState = RegEx.getInitialState(input);
        Field field = new Field(initialState, rules);
        System.out.println(field.toString());
        int lastSum = field.getSumOfPlants();
        for (int i = 0; i < 125; i++) {
            field.update();
            System.out.println(field.toString());
            System.out.println(i);
        }
        System.out.println(field.getSumOfPlants() + " + (50 * 10^9 - 125)" + field.getNumberOfPlants());
    }
}

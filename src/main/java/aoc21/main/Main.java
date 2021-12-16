package aoc21.main;

import aoc.days.Day;
import aoc21.days.day1.Day1;
import aoc21.days.day10.Day10;
import aoc21.days.day11.Day11;
import aoc21.days.day13.Day13;
import aoc21.days.day14.Day14;
import aoc21.days.day15.Day15;
import aoc21.days.day16.Day16;
import aoc21.days.day2.Day2;
import aoc21.days.day3.Day3;
import aoc21.days.day4.Day4;
import aoc21.days.day5.Day5;
import aoc21.days.day6.Day6;
import aoc21.days.day7.Day7;
import aoc21.days.day8.Day8;
import aoc21.days.day9.Day9;

public class Main {
    private static final Day[] days =
            {new Day1(), new Day2(), new Day3(), new Day4(), new Day5(), new Day6(), new Day7(), new Day8(), new Day9()
            ,new Day10(), new Day11(), null, new Day13(), new Day14(), new Day15(), new Day16()};

    private static void runDay(int dayNumber) {
        days[dayNumber - 1].runAndPrint("input" + dayNumber);
    }

    public static void main(String[] args) {
        runDay(16);
    }
}

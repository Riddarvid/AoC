package aoc21.main;

import aoc.days.Day;
import aoc21.days.day1.Day1;
<<<<<<< HEAD
import aoc21.days.day10.Day10;
=======
import aoc21.days.day11.Day11;
>>>>>>> b90bafc1038c1d3bc1f9779123e3e53df2113488
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
            ,new Day10(), new Day11()};

    private static void runDay(int dayNumber) {
        days[dayNumber - 1].runAndPrint("input" + dayNumber);
    }

    public static void main(String[] args) {
        runDay(11);
    }
}

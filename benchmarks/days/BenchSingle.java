package days;

import days.day1.Day1;
import days.day10.Day10;
import days.day11.Day11;
import days.day12.Day12;
import days.day13.Day13;
import days.day14.Day14;
import days.day15.Day15;
import days.day16.Day16;
import days.day17.Day17;
import days.day18.Day18;
import days.day19.Day19;
import days.day2.Day2;
import days.day23.Day23;
import days.day3.Day3;
import days.day4.Day4;
import days.day5.Day5;
import days.day6.Day6;
import days.day7.Day7;
import days.day8.Day8;
import days.day9.Day9;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;
import riddarvid.aoc.days.Day;

import java.util.InputMismatchException;
import java.util.concurrent.TimeUnit;

public class BenchSingle {
    private final static int DAY = 23;

    @Benchmark
    @BenchmarkMode({Mode.AverageTime})
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    public void setup(Day1State state, Blackhole blackhole) {
        state.dayFresh.setup();
        blackhole.consume(state.dayFresh);
    }

    @Benchmark
    @BenchmarkMode({Mode.AverageTime})
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    public void part1(Day1State state, Blackhole blackhole) {
        blackhole.consume(state.daySetupDone.part1());
    }

    @Benchmark
    @BenchmarkMode({Mode.AverageTime})
    @OutputTimeUnit(TimeUnit.NANOSECONDS)
    public void part2(Day1State state, Blackhole blackhole) {
        blackhole.consume(state.dayPart1Done.part2());
    }

    @State(Scope.Thread)
    public static class Day1State {
        public Day dayFresh;
        public Day daySetupDone;
        public Day dayPart1Done;

        public Day1State() {
            switch (DAY) {
                case 1 -> {
                    dayFresh = new Day1();
                    daySetupDone = new Day1();
                    dayPart1Done = new Day1();
                }
                case 2 -> {
                    dayFresh = new Day2();
                    daySetupDone = new Day2();
                    dayPart1Done = new Day2();
                }
                case 3 -> {
                    dayFresh = new Day3();
                    daySetupDone = new Day3();
                    dayPart1Done = new Day3();
                }
                case 4 -> {
                    dayFresh = new Day4();
                    daySetupDone = new Day4();
                    dayPart1Done = new Day4();
                }
                case 5 -> {
                    dayFresh = new Day5();
                    daySetupDone = new Day5();
                    dayPart1Done = new Day5();
                }
                case 6 -> {
                    dayFresh = new Day6();
                    daySetupDone = new Day6();
                    dayPart1Done = new Day6();
                }
                case 7 -> {
                    dayFresh = new Day7();
                    daySetupDone = new Day7();
                    dayPart1Done = new Day7();
                }
                case 8 -> {
                    dayFresh = new Day8();
                    daySetupDone = new Day8();
                    dayPart1Done = new Day8();
                }
                case 9 -> {
                    dayFresh = new Day9();
                    daySetupDone = new Day9();
                    dayPart1Done = new Day9();
                }
                case 10 -> {
                    dayFresh = new Day10();
                    daySetupDone = new Day10();
                    dayPart1Done = new Day10();
                }
                case 11 -> {
                    dayFresh = new Day11();
                    daySetupDone = new Day11();
                    dayPart1Done = new Day11();
                }
                case 12 -> {
                    dayFresh = new Day12();
                    daySetupDone = new Day12();
                    dayPart1Done = new Day12();
                }
                case 13 -> {
                    dayFresh = new Day13();
                    daySetupDone = new Day13();
                    dayPart1Done = new Day13();
                }
                case 14 -> {
                    dayFresh = new Day14();
                    daySetupDone = new Day14();
                    dayPart1Done = new Day14();
                }
                case 15 -> {
                    dayFresh = new Day15();
                    daySetupDone = new Day15();
                    dayPart1Done = new Day15();
                }
                case 16 -> {
                    dayFresh = new Day16();
                    daySetupDone = new Day16();
                    dayPart1Done = new Day16();
                }
                case 17 -> {
                    dayFresh = new Day17();
                    daySetupDone = new Day17();
                    dayPart1Done = new Day17();
                }
                case 18 -> {
                    dayFresh = new Day18();
                    daySetupDone = new Day18();
                    dayPart1Done = new Day18();
                }
                case 19 -> {
                    dayFresh = new Day19();
                    daySetupDone = new Day19();
                    dayPart1Done = new Day19();
                }
                case 23 -> {
                    dayFresh = new Day23();
                    daySetupDone = new Day23();
                    dayPart1Done = new Day23();
                }
                default -> throw new InputMismatchException("No such day");
            }
            daySetupDone.setup();
            dayPart1Done.setup();
            dayPart1Done.part1();
        }
    }
}

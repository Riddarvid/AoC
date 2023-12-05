package aoc19.days.day13;

import riddarvid.aoc.days.Day;
import aoc19.utils.input.InputUtils;

public class Day13 extends Day {
    private long[] memory;

    public static void main(String[] args) {
        new Day13();
    }

    @Override
    protected void part1() {
        Arcade arcade = new Arcade(memory, false);
        arcade.run();
        System.out.println(arcade.getNumberOf(Tile.BLOCK));
    }

    @Override
    protected void part2() {
        memory[0] = 2;
        Arcade arcade = new Arcade(memory, true);
        arcade.activateView();
        arcade.run();
        System.out.println(arcade.score());
    }

    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
    }
}

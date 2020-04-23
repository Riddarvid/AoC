package AoC.day23;

import AoC.Day;

public class Day23 extends Day {
    public static void main(String[] args) {
        new Day23();
    }

    private int collatz(int a) {
        int b = 0;
        while (a > 1) {
            b++;
            if (a % 2 == 0) {
                a = a / 2;
            } else {
                a = 3 * a + 1;
            }
        }
        return b;
    }

    @Override
    protected void part1() {
        System.out.println(collatz(20895));
    }

    @Override
    protected void part2() {
        System.out.println(collatz(60975));
    }

    @Override
    protected void setup() {

    }
}

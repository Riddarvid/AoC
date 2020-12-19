package days.day18;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day18 extends Day {
    private List<Expression> expressions;

    public static void main(String[] args) {
        new Day18().runAndPrint();
    }

    @Override
    public long part1() {
        long sum = 0;
        for (Expression expression : expressions) {
            sum += expression.evaluate1();
        }
        return sum;
    }

    @Override
    public long part2() {
        long sum = 0;
        for (Expression expression : expressions) {
            sum += expression.evaluate2();
        }
        return sum;
    }

    @Override
    public void setup() {
        expressions = new ArrayList<>();
        for (String s : lines) {
            expressions.add(new Expression(s));
        }
    }
}

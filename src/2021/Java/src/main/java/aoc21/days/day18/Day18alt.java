package aoc21.days.day18;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day18alt {
    /*private List<String> snailNumbers;

    @Override
    public void setup() {
        snailNumbers = new ArrayList<>(lines);
    }

    private String reduceNumber(List<SnailValue> snailNumber) {
        StringBuilder number = new StringBuilder(numberString);
        boolean wasReduced = true;
        while (wasReduced) {
            wasReduced = false;
            int depth = 0;
            //Explode if possible
            for (int index = 0; index < number.length(); index++) {
                char c = number.charAt(index);
                if (c == '[') {
                    depth++;
                } else if (c == ']') {
                    depth--;
                }
                if (depth == 5) {
                    explode(number, index);
                    wasReduced = true;
                    break;
                }
            }
            if (wasReduced) {
                continue;
            }
            for (int index = 0; index < number.length(); index++) {
                char c = number.charAt(index);
                if (Character.isDigit(c) && Character.isDigit(number.charAt(index + 1))) {
                    split(index);
                    wasReduced = true;
                    break;
                }
            }
        }
        return number.toString();
    }

    private void explode(StringBuilder number, int startIndex) {
        int commaIndex = startIndex + 1;
        while (number.charAt(commaIndex) != ',') {
            commaIndex++;
        }
        int endIndex = commaIndex + 1;
        while (number.charAt(endIndex) != ']') {
            endIndex++;
        }
        int firstValue
    }

    @Override
    public long part1() {
        return 0;
    }

    @Override
    public long part2() {
        return 0;
    }*/
}

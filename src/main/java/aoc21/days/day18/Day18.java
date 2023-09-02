package aoc21.days.day18;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day18 extends Day {
    private List<SnailNumber> snailNumbers;

    private SnailNumber parseSnailNumber(String s) {
        if (s.startsWith("[")) {
            return parsePair(s.substring(1, s.length() - 1));
        } else {
            return parseNumber(s);
        }
    }

    private SnailNumber parsePair(String s) {
        int index = 0;
        int depth = 0;
        while (!(s.charAt(index) == ',' && depth == 0)) {
            if (s.charAt(index) == '[') {
                depth++;
            } else if (s.charAt(index) == ']') {
                depth--;
            }
            index++;
        }
        SnailNumber firstNumber = parseSnailNumber(s.substring(0, index));
        int startIndexSecond = index + 1;
        while (index < s.length() && depth>= 0) {
            if (s.charAt(index) == '[') {
                depth++;
            } else if (s.charAt(index) == ']') {
                depth--;
            }
            index++;
        }
        SnailNumber secondNumber = parseSnailNumber(s.substring(startIndexSecond, index));
        return new Pair(firstNumber, secondNumber);
    }

    private SnailNumber parseNumber(String s) {
        return new RegularNumber(Integer.parseInt(s));
    }

    @Override
    public void setup() {
        snailNumbers = new ArrayList<>();
        for (String s : lines) {
            snailNumbers.add(parseSnailNumber(s));
        }
    }

    private LeftToRightReference generateReference(SnailNumber number) {
        LeftToRightReference reference = new LeftToRightReference();
        number.register(reference);
        return reference;
    }

    private SnailNumber reduce(SnailNumber number) {
        while (true) {
            LeftToRightReference reference = generateReference(number);
            number = number.tryExplode(0, reference);
            if (!SnailNumber.hasReduced) {
                number = number.trySplit();
                if (!SnailNumber.hasReduced) {
                    return number;
                }
            }
            //System.out.println(number.toStringColored(0));
            SnailNumber.hasReduced = false;
        }
    }

    private SnailNumber addSnailNumbers(SnailNumber first, SnailNumber second) {
        return new Pair(first.copy(), second.copy());
    }

    @Override
    public long part1() {
        SnailNumber sum = snailNumbers.get(0);
        for (int i = 1; i < snailNumbers.size(); i++) {
            sum = addSnailNumbers(sum, snailNumbers.get(i));
            //System.out.println(sum.toStringColored(0));
            sum = reduce(sum);
        }
        return sum.getMagnitude();
    }

    @Override
    public long part2() {
        long maxMagnitude = 0;
        for (int i = 0; i < snailNumbers.size(); i++) {
            for (int j = i + 1; j < snailNumbers.size(); j++) {
                SnailNumber sum = addSnailNumbers(snailNumbers.get(i), snailNumbers.get(j));
                sum = reduce(sum);
                long magnitude = sum.getMagnitude();
                if (magnitude > maxMagnitude) {
                    maxMagnitude = magnitude;
                }
                sum = addSnailNumbers(snailNumbers.get(j), snailNumbers.get(i));
                sum = reduce(sum);
                magnitude = sum.getMagnitude();
                if (magnitude > maxMagnitude) {
                    maxMagnitude = magnitude;
                }
            }
        }
        return maxMagnitude;
    }
}

package aoc.days.day9;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.List;

public class Day9 extends Day {
    private char[] data;

    public static void main(String[] args) {
        new Day9();
    }

    @Override
    protected void part1() {
        System.out.println(decompress(data).length);
    }

    private char[] decompress(char[] data) {
        StringBuilder sb = new StringBuilder();
        int i = 0;
        while (i < data.length) {
            while (i < data.length && data[i] != '(') {
                sb.append(data[i]);
                i++;
            }
            if (i >= data.length) {
                break;
            }
            int start = i;
            while (data[i] != ')') {
                i++;
            }
            List<Integer> values = InputUtils.getInts(data, start, i);
            int length = values.get(0);
            int times = values.get(1);
            char[] toRepeat = new char[length];
            System.arraycopy(data, i + 1, toRepeat, 0, length);
            sb.append(String.valueOf(toRepeat).repeat(times));
            i += length + 1;
        }
        return sb.toString().toCharArray();
    }

    @Override
    protected void part2() {
        System.out.println(length(data));
    }

    private long length(char[] data) {
        if (data.length == 0) {
            return 0;
        }
        if (!contains(data, '(')) {
            return data.length;
        }
        long sum;
        int i = 0;
        while (data[i] != '(') {
            i++;
        }
        sum = i;
        int start = i;
        while (data[i] != ')') {
            i++;
        }
        List<Integer> values = InputUtils.getInts(data, start, i);
        int length = values.get(0);
        int times = values.get(1);
        char[] toRepeat = new char[length];
        System.arraycopy(data, i + 1, toRepeat, 0, length);
        sum += times * length(toRepeat);
        i += length + 1;
        char[] newData = new char[data.length - i];
        System.arraycopy(data, i, newData, 0, data.length - i);
        sum += length(newData);
        return sum;
    }

    private boolean contains(char[] data, char target) {
        for (char c : data) {
            if (c == target) {
                return true;
            }
        }
        return false;
    }

    @Override
    protected void setup() {
        data = lines.get(0).toCharArray();
    }
}

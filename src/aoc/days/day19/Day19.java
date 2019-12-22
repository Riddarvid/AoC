package aoc.days.day19;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.Communicator;

public class Day19 extends Day {
    long[] memory;

    public static void main(String[] args) {
        new Day19();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (int y = 0; y < 50; y++) {
            for (int x = 0; x < 50; x++) {
                Communicator cm = new TractorCommunicator(memory);
                Thread t1 = new Thread(cm);
                t1.setDaemon(true);
                t1.start();
                if (inRange(x, y)) {
                    sum++;
                }
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        int row = findRow();
        int pos = 0;
        while (!isDone(pos, row)) {
            pos++;
        }
        System.out.println(pos * 10000 + row);
    }

    private int findRow() {
        int pos = 0;
        int row = 10;
        for (;; row++) {
            //System.out.println(row);
            while (!inRange(pos, row)) {
                pos++;
            }
            while (inRange(pos + 100, row)) {
                pos++;
            }
            if (isDone(pos, row)) {
                break;
            }
        }
        return row;
    }

    private boolean isDone(int x, int y) {
        return inRange(x, y) && inRange(x + 99, y) && inRange(x, y + 99) && inRange(x + 99, y +99);
    }

    private boolean inRange(int x, int y) {
        TractorCommunicator cm = new TractorCommunicator(memory);
        Thread t1 = new Thread(cm);
        t1.setDaemon(true);
        t1.start();
        cm.makeRequest(x);
        cm.makeRequest(y);
        return cm.getOutput() == 1;
    }

    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
    }
}

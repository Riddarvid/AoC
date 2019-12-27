package aoc.days.day25;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.math.MathUtils;

import java.util.*;

public class Day25 extends Day {
    long[] memory;

    public static void main(String[] args) {
        new Day25();
    }

    @Override
    protected void part1() {
        DroneCommunicator dc = new DroneCommunicator(memory);
        Thread t = new Thread(dc);
        t.setDaemon(true);
        t.start();
        Scanner sc = new Scanner(System.in);
        autoplay(dc);
        while (dc.isRunning()) {
            String input = sc.nextLine();
            dc.enterCommand(input);
        }
    }

    private void autoplay(DroneCommunicator dc) {
        dc.enterCommand("north");
        dc.enterCommand("north");
        dc.enterCommand("take sand");
        dc.enterCommand("south");
        dc.enterCommand("south");
        dc.enterCommand("south");
        dc.enterCommand("take space heater");
        dc.enterCommand("south");
        dc.enterCommand("east");
        dc.enterCommand("take loom");
        dc.enterCommand("west");
        dc.enterCommand("north");
        dc.enterCommand("west");
        dc.enterCommand("take wreath");
        dc.enterCommand("south");
        dc.enterCommand("take space law space brochure");
        dc.enterCommand("south");
        dc.enterCommand("take pointer");
        dc.enterCommand("north");
        dc.enterCommand("north");
        dc.enterCommand("east");
        dc.enterCommand("north"); //Back at hull breach
        dc.enterCommand("west");
        dc.enterCommand("south");
        dc.enterCommand("take planetoid");
        dc.enterCommand("north");
        dc.enterCommand("west");
        dc.enterCommand("take festive hat");
        dc.enterCommand("south");
        dc.enterCommand("west");
        dc.enterCommand("drop sand");
        dc.enterCommand("drop space heater");
        dc.enterCommand("drop loom");
        dc.enterCommand("drop wreath");
        dc.enterCommand("drop space law space brochure");
        dc.enterCommand("drop pointer");
        dc.enterCommand("drop planetoid");
        dc.enterCommand("drop festive hat");
        findCombo(dc);
    }

    private void findCombo(DroneCommunicator dc) { //Metoden utgår från att du har alla items och befinner dig i Security Checkpoint
        Set<Integer> items = new HashSet<>();
        items.add(0);
        items.add(1);
        items.add(2);
        items.add(3);
        items.add(4);
        items.add(5);
        items.add(6);
        items.add(7);
        for (int i = 1; i <= 8; i++) {
            Set<Set<Integer>> combinations = MathUtils.generateCombinations(items, i);
            for (Set<Integer> combination : combinations) {
                dc.inputCombination(combination);
            }
        }

    }

    @Override
    protected void part2() {

    }

    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
    }
}

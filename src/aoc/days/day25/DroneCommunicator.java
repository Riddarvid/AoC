package aoc.days.day25;

import aoc.utils.intcode.Communicator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DroneCommunicator extends Communicator {
    private Map<Integer, String> nameMap;

    public DroneCommunicator(long[] memory) {
        super(memory);
        nameMap = new HashMap<>();
        nameMap.put(0, "sand");
        nameMap.put(1, "space heater");
        nameMap.put(2, "loom");
        nameMap.put(3, "wreath");
        nameMap.put(4, "space law space brochure");
        nameMap.put(5, "pointer");
        nameMap.put(6, "planetoid");
        nameMap.put(7, "festive hat");
    }

    @Override
    public void output(long val) {
        System.out.print((char)(val));
    }

    public void enterCommand(String command) {
        for (char c : command.toCharArray()) {
            makeRequest(c);
        }
        makeRequest('\n');
    }

    public void inputCombination(Set<Integer> combination) {
        dropAll();
        for (int item : combination) {
            takeItem(item);
        }
        enterCommand("north");
    }

    private void dropAll() {
        for (int item = 0; item < 8; item++) {
            dropItem(item);
        }
    }

    private void dropItem(int item) {
        String command = "drop " + nameMap.get(item);
        enterCommand(command);
    }

    private void takeItem(int item) {
        String command = "take " + nameMap.get(item);
        enterCommand(command);
    }
}

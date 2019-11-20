package AoC.Day4;

import AoC.Action;
import AoC.FileUtilities;
import AoC.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Puzzle7 {
    public static void main(String[] args) {
        PriorityQueue<Entry> entries = new PriorityQueue<>();
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day4/Input.txt"));
        for (int i = 0; i < input.length; i++) {
            List<Integer> times = RegEx.getIntegers('[', ']', input[i]);
            Action action = RegEx.getAction(input[i]);
            PointInTime pointInTime = new PointInTime(times.get(0), times.get(1), times.get(2), times.get(3), times.get(4));
            int id;
            if (action == Action.START) {
                id = RegEx.getIntegers(']', input[i]).get(0);
            } else {
                id = -1;
            }
            Entry entry = new Entry(pointInTime, action, id);
            entries.add(entry);
        }
        List<Guard> guards = new ArrayList<>();
        Entry entry = entries.poll();
        while (!entries.isEmpty()) {
            Guard guard = new Guard(entry.getId());
            if (guards.contains(guard)) {
                guard = guards.get(guards.indexOf(guard));
            } else {
                guards.add(guard);
            }
            List<PointInTime> wakeSleep = new ArrayList<>();
            while ((!entries.isEmpty()) && ((entry = entries.poll()).getAction() != Action.START)) {
                wakeSleep.add(entry.getTime());
            }
            guard.addSleep(wakeSleep);
            wakeSleep.clear();
        }
        Queue<Guard> guardQueue = new PriorityQueue<>();
        guardQueue.addAll(guards);
        System.out.println(guardQueue.toString());
        System.out.println(guardQueue.poll().toString());
    }

    public static boolean contains(List<Guard> guards, int id) {
        for (Guard g : guards) {
            if (g.getId() == id) {
                return true;
            }
        }
        return false;
    }
}

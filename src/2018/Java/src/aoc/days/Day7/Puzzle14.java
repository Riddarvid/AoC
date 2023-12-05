package aoc.days.Day7;

import aoc.FileUtilities;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Puzzle14 {
    public static void main(String[] args) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day7/input.txt"));
        List<Integer>[] connections = Day7.getConnections(input);
        List<Elf> elves = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            elves.add(new Elf());
        }
        List<Integer> inProgress = new ArrayList<>();
        List<Integer> sequence = new ArrayList<>();
        int[] sum;
        int totalTime = 0;
        while (sequence.size() != 26) {
            sum = Day7.getSum(connections);
            int i = 0;
            for (Elf elf : elves) {
                if (!elf.hasTask()) {
                    while ((i < sum.length) && (sum[i] != 0 || sequence.contains(i) || inProgress.contains(i))) {
                        i++;
                    }
                    if (i < sum.length) {
                        inProgress.add(i);
                        elf.assignTask(i);
                    }
                }
            }
            for (Elf elf : elves) {
                if (elf.hasTask()) {
                    Integer task = elf.work();
                    if (task != null) {
                        sequence.add(task);
                        connections[task].clear();
                    }
                }
            }
            totalTime++;
            printProgress(elves);
        }
        System.out.println(Day7.intArrToString(sequence));
        System.out.println(totalTime);
    }

    public static void printProgress(List<Elf> elves) {
        StringBuilder sb = new StringBuilder();
        for (Elf elf : elves) {
            Integer task = elf.getCurrentTask();
            Character cTask;
            if (task == null) {
                cTask = (char)(38);
            } else {
                cTask = (char)(task + 65);
            }
            Integer progress = elf.getProgress();
            int comp;
            if (progress < 10) {
                comp = 2;
            } else {
                comp = 1;
            }
            sb.append(cTask).append(':').append(progress);
            for (int i = 0; i < comp; i++) {
                sb.append(' ');
            }
        }
        System.out.println(sb.toString());
    }
}

package AoC.Day7;

import AoC.FileUtilities;
import AoC.RegEx;

import java.io.File;
import java.util.*;

public class Puzzle13 {
    public static void main(String[] args) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day7/Input.txt"));
        List<Integer>[] connections = Day7.getConnections(input);
        List<Integer> sequence = new ArrayList<>();
        int[] sum;
        while (sequence.size() != 26) {
            sum = Day7.getSum(connections);
            int i = 0;
            while (sum[i] != 0 || sequence.contains(i)) {
                i++;
            }
            sequence.add(i);
            connections[i] = new ArrayList<>();
        }
        System.out.println(Day7.intArrToString(sequence));
    }
}

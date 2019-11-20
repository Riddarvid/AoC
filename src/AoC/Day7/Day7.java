package AoC.Day7;

import AoC.RegEx;

import java.util.ArrayList;
import java.util.List;

public abstract class Day7 {

    public static List<Integer>[] getConnections(String[] input) {
        Integer[][] relations = getRelations(input);
        List<Integer>[] connections = new List[26];
        for (int i = 0; i <connections.length; i++) {
            connections[i] = new ArrayList<>();
        }
        for (Integer[] relation : relations) {
            connections[relation[0]].add(relation[1]);
        }
        return connections;
    }

    public static String intArrToString(List<Integer> sequence) {
        StringBuilder sb = new StringBuilder();
        for (Integer i : sequence) {
            sb.append(intToChar(i));
        }
        return sb.toString();
    }

    public static char intToChar(int i) {
        return (char)(i + 65);
    }

    public static void remove(int target, List<Integer>[] connections) {
        for (List<Integer> list : connections) {
            if (list.contains(target)) {
                list.remove(target);
            }
        }
    }

    public static int[] getSum(List<Integer>[] connections) {
        int[] sum = new int[26];
        for (int i = 0; i < sum.length; i++) {
            sum[i] = getSumSingle(i, connections);
        }
        return sum;
    }

    public static int getSumSingle(int target, List<Integer>[] connections) {
        int sum = 0;
        for (List<Integer> list : connections) {
            if (list.contains(target)) {
                sum++;
            }
        }
        return sum;
    }

    public static Integer[][] getRelations(String[] input) {
        Integer[][] relations = new Integer[input.length][2];
        for (int i = 0; i < input.length; i++) {
            relations[i] = charToInt(RegEx.getNodes(input[i]));
        }
        return relations;
    }

    public static Integer[] charToInt(Character[] chars) {
        Integer[] ints = new Integer[chars.length];
        for (int i = 0; i < chars.length; i++) {
            ints[i] = chars[i] - 65;
        }
        return ints;
    }
}

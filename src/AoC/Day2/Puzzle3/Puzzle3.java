package AoC.Day2.Puzzle3;

import AoC.FileUtilities;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class Puzzle3 {

    public static void main(String[] args) {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Puzzle3/Input.txt"));
        Map<Character, Integer> map = new HashMap<>();
        int doubles = 0;
        int triples = 0;
        for (String s : input) {
            map.clear();
            for (Character c : s.toCharArray()) {
                if (map.containsKey(c)) {
                    map.put(c, map.get(c) + 1);
                } else {
                    map.put(c, 1);
                }
            }
            if (hasRepeats(map, s, 2)) {
                doubles++;
            }
            if (hasRepeats(map, s, 3)) {
                triples++;
            }
        }
        System.out.println(doubles * triples);
    }

    public static boolean hasRepeats(Map<Character, Integer> map, String string, int n) {
        boolean hasRepeat = false;
        for (Character c : string.toCharArray()) {
            if (map.get(c) == n) {
                hasRepeat = true;
            }
        }
        return hasRepeat;
    }
}

package aoc.days;

import java.util.ArrayList;
import java.util.List;

public class KnotHash {

    public static String hash(String key) {
        int[] knot = getKnot();
        List<Integer> lengths = getLengths(key);
        int current = 0;
        int skipSize = 0;
        for (int i = 0; i < 64; i++) {
            for (int length : lengths) {
                reverse(knot, current, length);
                current += length + skipSize;
                skipSize++;
            }
        }
        int[] denseHash = createDenseHash(knot);
        return toHex(denseHash);
    }

    private static int[] createDenseHash(int[] sparseHash) {
        int[] denseHash = new int[sparseHash.length / 16];
        for (int i = 0; i < sparseHash.length / 16; i++) {
            denseHash[i] = createDenseHash(sparseHash, i * 16);
        }
        return denseHash;
    }

    private static int createDenseHash(int[] hash, int index) {
        int n = hash[index];
        for (int i = index + 1; i < index + 16; i++) {
            n ^= hash[i];
        }
        return n;
    }

    private static String toHex(int[] denseHash) {
        StringBuilder hex = new StringBuilder();
        for (int n : denseHash) {
            hex.append(String.format("%02x", n));
        }
        return hex.toString();
    }

    private static void reverse(int[] knot, int current, int length) {
        int startIndex = current;
        int endIndex = current + length - 1;
        while (endIndex > startIndex) {
            int temp = knot[endIndex % knot.length];
            knot[endIndex % knot.length] = knot[startIndex % knot.length];
            knot[startIndex % knot.length] = temp;
            endIndex--;
            startIndex++;
        }
    }

    private static int[] getKnot() {
        int[] knot = new int[256];
        for (int i = 0; i < knot.length; i++) {
            knot[i] = i;
        }
        return knot;
    }

    private static List<Integer> getLengths(String key) {
        List<Integer> lengths = new ArrayList<>();
        for (char c : key.toCharArray()) {
            lengths.add((int)c);
        }
        lengths.add(17);
        lengths.add(31);
        lengths.add(73);
        lengths.add(47);
        lengths.add(23);
        return lengths;
    }
}

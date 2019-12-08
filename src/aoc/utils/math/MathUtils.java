package aoc.utils.math;

import java.util.ArrayList;
import java.util.List;

public class MathUtils {
    public static List<List<Integer>> generatePermutations(List<Integer> elements) {
        return generatePermutations(elements, new ArrayList<>());
    }

    private static List<List<Integer>> generatePermutations(List<Integer> elements, List<Integer> soFar) {
        List<List<Integer>> output = new ArrayList<>();
        if (soFar.size() == elements.size()) {
            output.add(soFar);
        } else {
            for (int element : elements) {
                List<Integer> next = new ArrayList<>(soFar);
                if (isValid(element, elements, soFar)) {
                    next.add(element);
                    output.addAll(generatePermutations(elements, next));
                }
            }
        }
        return output;
    }

    private static boolean isValid(int element, List<Integer> elements, List<Integer> soFar) {
        return (count(element, soFar) < count(element, elements));
    }

    private static int count(int target, List<Integer> elements) {
        int count = 0;
        for (int element : elements) {
            if (element == target) {
                count++;
            }
        }
        return count;
    }
}

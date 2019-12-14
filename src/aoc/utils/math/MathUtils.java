package aoc.utils.math;

import java.util.ArrayList;
import java.util.List;

public class MathUtils {
    public static List<List<Integer>> generatePermutations(List<Integer> elements) {
        return generatePermutations(elements, new ArrayList<>());
    }

    public static long lcm(long a, long b) {
        List<Long> aPrimes = getPrimes(a);
        List<Long> bPrimes = getPrimes(b);
        List<Long> factors = removeDuplicates(aPrimes, bPrimes);
        return product(factors);
    }

    private static List<Long> removeDuplicates(List<Long> aPrimes, List<Long> bPrimes) {
        List<Long> toRemove = new ArrayList<>();
        for (long prime : aPrimes) {
            if (bPrimes.contains(prime)) {
                toRemove.add(prime);
                bPrimes.remove(prime);
            }
        }
        aPrimes.removeAll(toRemove);
        aPrimes.addAll(bPrimes);
        return aPrimes;
    }

    private static List<Long> getPrimes(long n) {
        List<Long> primes = new ArrayList<>();
        while (n > 1) {
            for (long factor = 2; factor <= n; factor++) {
                if (n % factor == 0) {
                    primes.add(factor);
                    n /= factor;
                    break;
                }
            }
        }
        return primes;
    }

    private static long product(List<Long> factors) {
        if (factors.isEmpty()) {
            return 0;
        }
        long product = 1;
        for (long factor : factors) {
            product *= factor;
        }
        return product;
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

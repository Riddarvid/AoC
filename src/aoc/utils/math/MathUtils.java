package aoc.utils.math;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class MathUtils {
    public static <T> List<List<T>> generatePermutations(List<T> elements) {
        return generatePermutations(elements, new ArrayList<>());
    }

    public static <T> List<List<T>> generateValidPermutations(List<T> elements, Map<T, Set<T>> requirements) {
        return generateValidPermutations(elements, new ArrayList<>(), requirements);
    }

    private static <T> List<List<T>> generateValidPermutations(List<T> elements, List<T> soFar, Map<T, Set<T>> requirements) {
        List<List<T>> output = new ArrayList<>();
        if (soFar.size() == elements.size() - 1) {
            System.out.println("Yeet");
        }
        if (soFar.size() == elements.size()) {
            output.add(soFar);
        } else {
            List<T> validElements = generateValidElements(elements, soFar, requirements);
            for (T element : validElements) {
                List<T> next = new ArrayList<>(soFar);
                next.add(element);
                output.addAll(generateValidPermutations(elements, next, requirements));
            }
        }
        return output;
    }

    private static <T> List<T> generateValidElements(List<T> elements, List<T> soFar, Map<T, Set<T>> requirements) {
        List<T> validElements = new ArrayList<>();
        for (T element : elements) {
            if (isValid(element, elements, soFar, requirements)) {
                validElements.add(element);
            }
        }
        return validElements;
    }

    private static <T> boolean isValid(T element, List<T> elements, List<T> soFar, Map<T, Set<T>> requirements) {
        boolean elementsLeft = count(element, soFar) < count(element, elements);
        boolean fulfillsRequirements = soFar.containsAll(requirements.get(element));
        return elementsLeft && fulfillsRequirements;
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

    private static <T> List<List<T>> generatePermutations(List<T> elements, List<T> soFar) {
        List<List<T>> output = new ArrayList<>();
        if (soFar.size() == elements.size()) {
            output.add(soFar);
        } else {
            for (T element : elements) {
                List<T> next = new ArrayList<>(soFar);
                if (isValid(element, elements, soFar)) {
                    next.add(element);
                    output.addAll(generatePermutations(elements, next));
                }
            }
        }
        return output;
    }

    private static <T> boolean isValid(T element, List<T> elements, List<T> soFar) {
        return (count(element, soFar) < count(element, elements));
    }

    private static <T> int count(T target, List<T> elements) {
        int count = 0;
        for (T element : elements) {
            if (element.equals(target)) {
                count++;
            }
        }
        return count;
    }
}

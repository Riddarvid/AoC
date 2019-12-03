package aoc.days.day23;

import aoc.days.Day;
import aoc.days.day23.instructions.*;
import aoc.utils.InputUtilities;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day23 extends Day {
    private List<Instruction> instructions;
    private List<Integer> primes;

    public static void main(String[] args) {
        new Day23();
    }

    @Override
    protected void part1() {
        Executer executer = new Executer(new VM(), instructions);
        executer.run();
    }

    @Override
    protected void part2() {
        int c = 126300;
        generatePrimes(c);
        int h = 0;
        for (int b = 109300; b <= c; b += 17) {
            if (!isPrime(b)) {
                h++;
            }
        }
        System.out.println(h);
    }

    private void generatePrimes(int n) {
        primes = new ArrayList<>();
        primes.add(2);
        for (int potential = 3; potential <= n; potential++) {
            if (!isDivisibleBy(potential, primes)) {
                primes.add(potential);
            }
        }
    }

    private boolean isDivisibleBy(int potential, List<Integer> primes) {
        for (int prime : primes) {
            if (potential % prime == 0) {
                return true;
            }
        }
        return false;
    }

    private boolean isPrime(int n) {
        return primes.contains(n);
    }

    @Override
    protected void setup() {
        instructions = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = InputUtilities.getTokens(s, ' ');
            switch (tokens.get(0)) {
                case "set":
                    instructions.add(new Set(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "sub":
                    instructions.add(new Sub(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "mul":
                    instructions.add(new Mul(tokens.get(1).charAt(0), tokens.get(2)));
                    break;
                case "jnz":
                    instructions.add(new Jnz(tokens.get(1), tokens.get(2)));
                    break;
                default:
                    throw new InputMismatchException("Invalid instruction");
            }
        }
    }
}

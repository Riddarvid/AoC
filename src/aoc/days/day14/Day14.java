package aoc.days.day14;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.*;

public class Day14 extends Day {
    private Set<String> chemicals;
    private Map<String, Requirement> requirements;

    public static void main(String[] args) {
        new Day14();
    }

    @Override
    protected void part1() {
        System.out.println(produceNFuel(1));
    }

    private long produceNFuel(long n) {
        Map<String, Long> required = new HashMap<>();
        Map<String, Long> leftovers = new HashMap<>();
        for (String chemical : chemicals) {
            leftovers.put(chemical, 0L);
        }
        required.put("FUEL", n);
        while (!done(required)) {
            String current;
            Iterator<String> it = required.keySet().iterator();
            do {
                current = it.next();
            } while (current.equals("ORE"));
            long needed = required.get(current);
            long available = leftovers.get(current);
            Requirement requirement = requirements.get(current);
            int factor = requirement.getOutput().getAmount();
            if (available < needed) { //If not enough leftover
                leftovers.put(current, 0L);
                needed -= available;
                long toProduce = (long)Math.ceil((double)(needed)/(double)(factor));
                create(leftovers, required, current, toProduce);
                balance(required, leftovers);
            } else {
                throw new InputMismatchException("Hmmm");
            }
        }
        return required.get("ORE");
    }

    private void balance(Map<String, Long> required, Map<String, Long> leftovers) {
        Iterator<String> it = required.keySet().iterator();
        while (it.hasNext()){
            String chemical = it.next();
            long needed = required.get(chemical);
            long available = leftovers.get(chemical);
            if (needed > available) {
                required.put(chemical, needed - available);
                leftovers.put(chemical, 0L);
            } else {
                leftovers.put(chemical, available - needed);
                it.remove();
            }
        }
    }

    private void create(Map<String, Long> leftovers, Map<String, Long> required,  String current, long toProduce) {
        Requirement requirement = requirements.get(current);
        for (Quantity quantity : requirement.getInputs()) {
            long before = required.getOrDefault(quantity.getName(), 0L);
            required.put(quantity.getName(), before + quantity.getAmount() * toProduce);
        }
        long before = leftovers.get(current);
        leftovers.put(current, before + requirement.getOutput().getAmount() * toProduce);
    }

    private boolean done(Map<String, Long> required) {
        for (String chemical : required.keySet()) {
            if (!chemical.equals("ORE") && required.get(chemical) > 0) {
                return false;
            }
        }
        return true;
    }

    @Override
    protected void part2() {
        int fuelCount = 1;
        long oreCount;
        do {
            fuelCount *= 10;
            oreCount = produceNFuel(fuelCount);
            System.out.println("Fuel: " + fuelCount + " => Ore: " + oreCount);
        } while (oreCount < 1000000000000L);
        fuelCount = findBest(fuelCount / 10, fuelCount);
        System.out.println(fuelCount - 1);
    }

    private int findBest(int low, int high) {
        long target = 1000000000000L;
        if (low == high) {
            return low;
        }
        int middle = low + (high - low) / 2;
        long oreCount = produceNFuel(middle);
        if (low == high - 1) {
            if (oreCount < target) {
                return high;
            } else {
                return low;
            }
        }
        System.out.println("Fuel: " + middle + " => Ore: " + oreCount);
        if (oreCount > target) {
            return findBest(low, middle);
        } else if (oreCount < target) {
            return findBest(middle, high);
        } else {
            return middle;
        }
    }

    @Override
    protected void setup() {
        chemicals = new HashSet<>();
        requirements = new HashMap<>();
        for (String s : lines) {
            s = s.replace(",", "");
            s = s.replace("=>", "");
            List<String> tokens = InputUtils.getTokens(s, ' ');
            List<Quantity> inputs = new ArrayList<>();
            for (int i = 0; i < tokens.size() - 2; i += 2) {
                inputs.add(new Quantity(tokens.get(i + 1), Integer.parseInt(tokens.get(i))));
                chemicals.add(tokens.get(i + 1));
            }
            Quantity output = new Quantity(tokens.get(tokens.size() - 1), Integer.parseInt(tokens.get(tokens.size() - 2)));
            chemicals.add(tokens.get(tokens.size() - 1));
            requirements.put(output.getName(), new Requirement(output, inputs));
        }
    }
}

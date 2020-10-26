package aoc.days.Day14;

import java.util.ArrayList;
import java.util.List;

public class Day14 {
    public static void main(String[] args) {
        new Day14();
    }

    public Day14() {
        //Puzzle1
        /*
        int input = 607331;
        List<Integer> recipes = new ArrayList<>();
        recipes.add(3);
        recipes.add(7);
        int elf1 = 0;
        int elf2 = 1;
        //List<Integer> elves = new ArrayList<>();
        //elves.add(elf1);
        //elves.add(elf2);
        while (recipes.size() < input + 10) {
            int sum = recipes.get(elf1) + recipes.get(elf2);
            if (sum > 9) {
                recipes.add(sum / 10);
            }
            recipes.add(sum % 10);
            elf1 = (elf1 + 1 + recipes.get(elf1)) % recipes.size();
            elf2 = (elf2 + 1 + recipes.get(elf2)) % recipes.size();
        }
        for (int i = input; i < input + 10; i++) {
            System.out.print(recipes.get(i));
        }
        */
        String input = "768071";
        List<Integer> recipes = new ArrayList<>();
        recipes.add(3);
        recipes.add(7);
        int elf1 = 0;
        int elf2 = 1;
        while (!endsWith(recipes, input)) {
            int sum = recipes.get(elf1) + recipes.get(elf2);
            if (sum > 9) {
                recipes.add(sum / 10);
                if (endsWith(recipes, input)) {
                    break;
                }
            }
            recipes.add(sum % 10);
            elf1 = (elf1 + 1 + recipes.get(elf1)) % recipes.size();
            elf2 = (elf2 + 1 + recipes.get(elf2)) % recipes.size();
        }
        System.out.println(recipes.size() - input.length());
    }

    private boolean endsWith(List<Integer> recipes, String input) {
        if (input.length() > recipes.size()) {
            return false;
        }
        for (int i = 0; i < input.length(); i++) {
            if (Character.getNumericValue(input.charAt(i)) != recipes.get(i + recipes.size() - input.length())) {
                return false;
            }
        }
        return true;
    }
}

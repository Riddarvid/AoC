package days.day21;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.*;
import java.util.stream.Collectors;

public class Day21 extends Day {
    private List<Food> foods;
    private Set<String> allAllergens;
    private Set<String> allIngredients;

    private Set<String> allergenicIngredients;

    public static void main(String[] args) {
        new Day21().runAndPrint();
    }

    @Override
    public long part1() {
        Set<String> nonAllergenic = new HashSet<>(allIngredients);
        for (String allergen : allAllergens) {
            nonAllergenic.retainAll(getNonAllergenic(allergen, allIngredients));
        }
        allergenicIngredients = new HashSet<>(allIngredients);
        allergenicIngredients.removeAll(nonAllergenic);
        int count = 0;
        for (String ingredient : nonAllergenic) {
            for (Food food : foods) {
                if (food.containsIngredient(ingredient)) {
                    count++;
                }
            }
        }
        return count;
    }

    private Collection<String> getNonAllergenic(String allergen, Set<String> ingredients) {
        Set<String> possiblyAllergenic = getPossiblyAllergenic(allergen, ingredients);
        Set<String> nonAllergenic = new HashSet<>(ingredients);
        nonAllergenic.removeAll(possiblyAllergenic);
        return nonAllergenic;
    }

    private Set<String> getPossiblyAllergenic(String allergen, Set<String> ingredients) {
        Set<String> possiblyAllergenic = new HashSet<>(ingredients);
        for (Food food : foods) {
            if (food.containsAllergen(allergen)) {
                possiblyAllergenic.retainAll(food.getIngredients());
            }
        }
        return possiblyAllergenic;
    }

    @Override
    public long part2() {
        Map<String, Set<String>> possibleIngredients = new HashMap<>();
        for (String allergen : allAllergens) {
            possibleIngredients.put(allergen, getPossiblyAllergenic(allergen, allergenicIngredients));
        }
        Map<String, String> allergenToIngredient = new HashMap<>();
        while (allergenToIngredient.size() != allAllergens.size()) {
            Set<String> toRemove = new HashSet<>();
            for (String allergen : possibleIngredients.keySet()) {
                if (possibleIngredients.get(allergen).size() == 1) {
                    String ingredient = possibleIngredients.get(allergen).iterator().next();
                    allergenToIngredient.put(allergen, ingredient);
                    toRemove.add(ingredient);
                }
            }
            for (String ingredient : toRemove) {
                for (Set<String> possible : possibleIngredients.values()) {
                    possible.remove(ingredient);
                }
            }
        }
        List<String> allergensSorted = allAllergens.stream().sorted().collect(Collectors.toList());
        StringBuilder sb = new StringBuilder();
        for (String allergen : allergensSorted) {
            sb.append(allergenToIngredient.get(allergen)).append(',');
        }
        sb.deleteCharAt(sb.length() - 1);
        System.out.println(sb.toString());
        return 0;
    }

    @Override
    public void setup() {
        foods = new ArrayList<>();
        allAllergens = new HashSet<>();
        allIngredients = new HashSet<>();
        for (String s : lines) {
            foods.add(parseFood(s));
        }
    }

    private Food parseFood(String s) {
        List<String> tokens = ParsingUtils.getTokens(s, '(');
        List<String> ingredients = ParsingUtils.getTokens(tokens.get(0), ' ');
        allIngredients.addAll(ingredients);
        String allergenString = tokens.get(1);
        allergenString = allergenString.substring(9);
        allergenString = allergenString.replace(",", "");
        allergenString = allergenString.substring(0, allergenString.length() - 1);
        List<String> allergens = ParsingUtils.getTokens(allergenString, ' ');
        this.allAllergens.addAll(allergens);
        return new Food(ingredients, allergens);
    }
}

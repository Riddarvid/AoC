package aoc15.days.day15;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day15 extends Day {
    private List<Ingredient> ingredients;

    @Override
    public long part2() {
        int maxScore = 0;
        List<List<Integer>> ratios = generateRatios(ingredients.size(), 100);
        for (List<Integer> ratio : ratios) {
            if (getCalories(ratio, ingredients.size()) != 500) {
                continue;
            }
            int current = getScore(ratio, ingredients.size());
            if (current > maxScore) {
                maxScore = current;
            }
        }
        return maxScore;
    }

    private int getScore(List<Integer> ratio, int numberOfIngredients) {
        int capacity = 0;
        int durability = 0;
        int flavor = 0;
        int texture = 0;
        for (int i = 0; i < numberOfIngredients; i++) {
            Ingredient ingredient = ingredients.get(i);
            int amount = ratio.get(i);
            capacity += ingredient.getCapacity() * amount;
            durability += ingredient.getDurability() * amount;
            flavor += ingredient.getFlavor() * amount;
            texture += ingredient.getTexture() * amount;
        }
        if (capacity <= 0 || durability <= 0 || flavor <= 0 || texture <= 0) {
            return 0;
        }
        return capacity * durability * flavor * texture;
    }

    private int getCalories(List<Integer> ratio, int numberOfIngredients) {
        int sum = 0;
        for (int i = 0; i < numberOfIngredients; i++) {
            int amount = ratio.get(i);
            sum += ingredients.get(i).getCalories() * amount;
        }
        return sum;
    }

    private List<List<Integer>> generateRatios(int numberOfIngredients, int totalAmount) {
        return generateRatios(new ArrayList<>(), 0, numberOfIngredients, totalAmount);
    }

    private List<List<Integer>> generateRatios(List<Integer> ratioSoFar, int ratioSoFarSum, int numberOfIngredients, int totalAmount) {
        List<List<Integer>> ratios = new ArrayList<>();
        if (ratioSoFar.size() == numberOfIngredients - 1) {
            List<Integer> ratio = new ArrayList<>(ratioSoFar);
            ratio.add(totalAmount - ratioSoFarSum);
            ratios.add(ratio);
            return ratios;
        }
        for (int amount = 0; amount < totalAmount - ratioSoFarSum; amount++) {
            List<Integer> newRatioSoFar = new ArrayList<>(ratioSoFar);
            newRatioSoFar.add(amount);
            ratios.addAll(generateRatios(newRatioSoFar, ratioSoFarSum + amount, numberOfIngredients, totalAmount));
        }
        return ratios;
    }

    @Override
    public long part1() {
        int maxScore = 0;
        List<List<Integer>> ratios = generateRatios(ingredients.size(), 100);
        for (List<Integer> ratio : ratios) {
            int current = getScore(ratio, ingredients.size());
            if (current > maxScore) {
                maxScore = current;
            }
        }
        return maxScore;
    }

    @Override
    public void setup() {
        ingredients = new ArrayList<>();
        for (String s : lines) {
            s = s.replace(",", "");
            s = s.replace(":", "");
            List<String > tokens = ParsingUtils.getTokens(s, ' ');
            Ingredient ingredient = new Ingredient(tokens.get(0), Integer.parseInt(tokens.get(2)), Integer.parseInt(tokens.get(4)), Integer.parseInt(tokens.get(6)), Integer.parseInt(tokens.get(8)), Integer.parseInt(tokens.get(10)));
            ingredients.add(ingredient);
        }
    }
}

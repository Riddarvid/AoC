package aoc15.days.day15;

import aoc.days.Day;
import aoc.math.geometry.VectorND;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day15alt extends Day {
    private List<IngredientAlt> ingredients;
    private int numberOfProperties;



    private int getScoreIgnoringCalories(VectorND ratio) {
        VectorND properties = VectorND.getNullVector(numberOfProperties);
        //For each ingredient, add all properties.
        for (int i = 0; i < ingredients.size(); i++) {
            IngredientAlt ingredient = ingredients.get(i);
            int amount = ratio.getCoordinate(i);
            properties = properties.add(ingredient.getProperties().scaleBy(amount));
        }
        int capacity = properties.getCoordinate(0);
        int durability = properties.getCoordinate(1);
        int flavor = properties.getCoordinate(2);
        int texture = properties.getCoordinate(3);
        if (capacity <= 0 || durability <= 0 || flavor <= 0 || texture <= 0) {
            return 0;
        }
        return capacity * durability * flavor * texture;
    }

    private int getCalories(VectorND ratio) {
        int sum = 0;
        for (int i = 0; i < ingredients.size(); i++) {
            IngredientAlt ingredient = ingredients.get(i);
            int amount = ratio.getCoordinate(i);
            sum += ingredient.getProperties().getCoordinate(4) * amount;
        }
        return sum;
    }

    private List<VectorND> generateRatios(int numberOfIngredients, int totalAmount) {
        return generateRatios(new VectorND(), 0, numberOfIngredients, totalAmount);
    }

    private List<VectorND> generateRatios(VectorND ratioSoFar, int ratioSoFarSum, int numberOfIngredients, int totalAmount) {
        List<VectorND> ratios = new ArrayList<>();
        if (ratioSoFar.getDimensions() == numberOfIngredients - 1) {
            VectorND newRatioSoFar = new VectorND(ratioSoFar, totalAmount - ratioSoFarSum);
            ratios.add(newRatioSoFar);
            return ratios;
        }
        for (int amount = 0; amount < totalAmount - ratioSoFarSum; amount++) {
            VectorND newRatioSoFar = new VectorND(ratioSoFar, amount);
            ratios.addAll(generateRatios(newRatioSoFar, ratioSoFarSum + amount, numberOfIngredients, totalAmount));
        }
        return ratios;
    }

    @Override
    public long part1() {
        int maxScore = 0;
        List<VectorND> ratios = generateRatios(ingredients.size(), 100);
        for (VectorND ratio : ratios) {
            int current = getScoreIgnoringCalories(ratio);
            if (current > maxScore) {
                maxScore = current;
            }
        }
        return maxScore;
    }

    @Override
    public long part2() {
        int maxScore = 0;
        List<VectorND> ratios = generateRatios(ingredients.size(), 100);
        for (VectorND ratio : ratios) {
            if (getCalories(ratio) != 500) {
                continue;
            }
            int current = getScoreIgnoringCalories(ratio);
            if (current > maxScore) {
                maxScore = current;
            }
        }
        return maxScore;
    }

    @Override
    public void setup() {
        numberOfProperties = 5;
        ingredients = new ArrayList<>();
        for (String s : lines) {
            s = s.replace(",", "");
            s = s.replace(":", "");
            List<String > tokens = ParsingUtils.getTokens(s, ' ');
            IngredientAlt ingredient = new IngredientAlt(tokens.get(0), Integer.parseInt(tokens.get(2)), Integer.parseInt(tokens.get(4)), Integer.parseInt(tokens.get(6)), Integer.parseInt(tokens.get(8)), Integer.parseInt(tokens.get(10)));
            ingredients.add(ingredient);
        }
    }
}

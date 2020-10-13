package AoC.day15;

import AoC.FileUtilities;
import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day15 extends Day {
    List<Ingredient> ingredients;

    public static void main(String[] args) {
        new Day15();
    }

    @Override
    protected void part2() {
        int maxScore = 0;
        int[] amounts = new int[4];
        for (int i1 = 0; i1 < 100; i1++) {
            for (int i2 = 0; i2 < 100 - i1; i2++) {
                for (int i3 = 0; i3 < 100 - (i1 + i2); i3++) {
                    amounts[0] = i1;
                    amounts[1] = i2;
                    amounts[2] = i3;
                    amounts[3] = 100 - (i1 + i2 + i3);
                    if (getCalories(amounts) != 500) {
                        continue;
                    }
                    int currentScore = getScore(amounts);
                    if (currentScore > maxScore) {
                        maxScore = currentScore;
                    }
                }
            }
        }
        System.out.println(maxScore);
    }

    private int getScore(int[] amounts) {
        int capacity = 0;
        int durability = 0;
        int flavor = 0;
        int texture = 0;
        for (int i = 0; i < 4; i++) {
            Ingredient ingredient = ingredients.get(i);
            capacity += ingredient.getCapacity() * amounts[i];
            durability += ingredient.getDurability() * amounts[i];
            flavor += ingredient.getFlavor() * amounts[i];
            texture += ingredient.getTexture() * amounts[i];
        }
        if (capacity <= 0 || durability <= 0 || flavor <= 0 || texture <= 0) {
            return 0;
        }
        return capacity * durability * flavor * texture;
    }

    private int getCalories(int[] amounts) {
        int sum = 0;
        for (int i = 0; i < 4; i++) {
            sum += ingredients.get(i).getCalories() * amounts[i];
        }
        return sum;
    }

    @Override
    protected void part1() {
        int maxScore = 0;
        int[] amounts = new int[4];
        for (int i1 = 0; i1 < 100; i1++) {
            for (int i2 = 0; i2 < 100 - i1; i2++) {
                for (int i3 = 0; i3 < 100 - (i1 + i2); i3++) {
                    amounts[0] = i1;
                    amounts[1] = i2;
                    amounts[2] = i3;
                    amounts[3] = 100 - (i1 + i2 + i3);
                    int current = getScore(amounts);
                    if (current > maxScore) {
                        maxScore = current;
                    }
                }
            }
        }
        System.out.println(maxScore);
    }

    @Override
    protected void setup() {
        ingredients = new ArrayList<>();
        for (String s : lines) {
            s = s.replace(',', ' ');
            s = s.replace(':', ' ');
            List<String > tokens = FileUtilities.getTokens(s, ' ');
            Ingredient ingredient = new Ingredient(tokens.get(0), Integer.parseInt(tokens.get(2)), Integer.parseInt(tokens.get(4)), Integer.parseInt(tokens.get(6)), Integer.parseInt(tokens.get(8)), Integer.parseInt(tokens.get(10)));
            ingredients.add(ingredient);
        }
    }
}

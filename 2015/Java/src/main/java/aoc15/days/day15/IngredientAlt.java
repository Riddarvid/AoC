package aoc15.days.day15;

import aoc.math.geometry.VectorND;

public class IngredientAlt {
    private final String name;
    private final VectorND properties;

    public IngredientAlt(String name, int capacity, int durability, int flavor, int texture, int calories) {
        this.name = name;
        properties = new VectorND(capacity, durability, flavor, texture, calories);
    }

    public VectorND getProperties() {
        return properties;
    }
}

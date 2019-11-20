package AoC.day15;

public class Ingredient {
    private final String name;
    private final int capacity;
    private final int durability;
    private final int flavor;
    private final int texture;
    private final int calories;


    public Ingredient(String name, int capacity, int durability, int flavor, int texture, int calories) {
        this.name = name;
        this.capacity = capacity;
        this.durability = durability;
        this.flavor = flavor;
        this.texture = texture;
        this.calories = calories;
    }

    @Override
    public String toString() {
        return name;
    }

    public String getName() {
        return name;
    }

    public int getCapacity() {
        return capacity;
    }

    public int getDurability() {
        return durability;
    }

    public int getFlavor() {
        return flavor;
    }

    public int getTexture() {
        return texture;
    }

    public int getCalories() {
        return calories;
    }
}

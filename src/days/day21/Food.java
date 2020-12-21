package days.day21;

import java.util.List;

public class Food {
    private final List<String> ingredients;
    private final List<String> allergens;

    public Food(List<String> ingredients, List<String> allergens) {
        this.ingredients = ingredients;
        this.allergens = allergens;
    }

    public boolean containsIngredient(String ingredient) {
        return ingredients.contains(ingredient);
    }

    public boolean containsAllergen(String allergen) {
        return allergens.contains(allergen);
    }

    public List<String> getIngredients() {
        return ingredients;
    }
}

package days.day2;

public class ShopPolicy extends Policy {
    public ShopPolicy(char letter, int low, int high) {
        super(letter, low, high);
    }

    @Override
    public boolean fulfills(String password) {
        boolean firstMatch = matches(password, getLow() - 1);
        boolean lastMatch = matches(password, getHigh() - 1);
        return (firstMatch && !lastMatch) || (!firstMatch && lastMatch);
    }

    private boolean matches(String password, int index) {
        return password.charAt(index) == getLetter();
    }
}

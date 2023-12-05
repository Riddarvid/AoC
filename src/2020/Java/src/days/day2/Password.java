package days.day2;

public class Password {
    private final String password;
    private final SledPolicy sledPolicy;
    private final ShopPolicy shopPolicy;

    public Password(String password, SledPolicy sledPolicy, ShopPolicy shopPolicy) {
        this.password = password;
        this.sledPolicy = sledPolicy;
        this.shopPolicy = shopPolicy;
    }

    public boolean fulfillsSledPolicy() {
        return sledPolicy.fulfills(password);
    }

    public boolean fulfillsShopPolicy() {
        return shopPolicy.fulfills(password);
    }
}

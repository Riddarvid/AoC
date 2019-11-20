package AoC.Day13;

public class CartApp {
    public static void main(String[] args) {
        Day13 model = new Day13();
        CartView view = new CartView(model, 900);
        CartController controller = new CartController(model, view);
        controller.run();
    }
}

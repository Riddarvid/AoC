package aoc.days.Day13;

public class CartController {
    Day13 model;
    CartView view;

    public CartController(Day13 model, CartView view) {
        this.model = model;
        this.view = view;
    }

    public void run() {
            while (model.carts.size() > 1) {
                //Thread.sleep(10);
                model.update();
                view.update();
            }
        System.out.println(model.carts.get(0).toString());
    }
}

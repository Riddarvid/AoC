package AoC.Day6;

public class Application {
    public static void main(String[] args) {
        int width = 800;
        int height = 800;
        Model model = new Model(width, height);
        View view = new View("Test", model, width, height);
    }
}

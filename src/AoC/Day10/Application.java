package AoC.Day10;

import AoC.FileUtilities;

import java.io.File;

public class Application {
    public static void main(String[] args) {
        new Application();
    }

    public Application() {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day10/Input.txt"));
        Model model = new Model(input);
        View view = new View("Test", model, 1000, 1000);
        Controller controller = new Controller(model, view);
    }
}

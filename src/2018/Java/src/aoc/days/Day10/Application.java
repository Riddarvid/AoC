package aoc.days.Day10;

import aoc.FileUtilities;

import java.io.File;

public class Application {
    public static void main(String[] args) {
        new Application();
    }

    public Application() {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day10/input.txt"));
        Model model = new Model(input);
        View view = new View("Test", model, 1000, 1000);
        Controller controller = new Controller(model, view);
    }
}

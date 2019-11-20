package AoC.Day1.Puzzle1;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Puzzle1 {
    public static void main(String[] args) {
        try {
            Scanner scanner = new Scanner(new File("src/AoC/Puzzle7/input.txt"));
            int sum = 0;
            while (scanner.hasNextLine()) {
                String row = scanner.nextLine();
                int n = Integer.parseInt(row);
                sum += n;
            }
            System.out.println(sum);
        } catch (FileNotFoundException e) {
            System.out.println("No such file");
        }
    }
}

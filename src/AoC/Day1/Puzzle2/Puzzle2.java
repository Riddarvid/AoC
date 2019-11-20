package AoC.Day1.Puzzle2;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Puzzle2 {


    public static void main(String[] args) {
        try {
            Scanner scanner = new Scanner(new File("src/AoC/Puzzle2/input.txt"));
            List found = new ArrayList();
            int sum = 0;
            while (scanner.hasNextLine()) {
                String row = scanner.nextLine();
                int n = Integer.parseInt(row);
                sum += n;
                if (found.contains(sum)) {
                    break;
                } else {
                    found.add(sum);
                }
                if (!scanner.hasNextLine()) {
                    scanner = new Scanner(new File("src/AoC/Puzzle2/input.txt"));
                }
            }
            System.out.println(sum);
        } catch (FileNotFoundException e) {
            System.out.println("No such file");
        }
    }
}

package AoC;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.InputMismatchException;
import java.util.Scanner;

public abstract class FileUtilities {

    public static String[] getArrayFromFile(File f) {
        try {
            Scanner sc = new Scanner(f);
            String[] arr = new String[getNumberOfLines(f)];
            int pos = 0;
            while (sc.hasNextLine()) {
                arr[pos] = sc.nextLine();
                pos++;
            }
            return arr;
        } catch (FileNotFoundException e) {
            throw new InputMismatchException();
        }
    }

    private static int getNumberOfLines(File f) {
        int count = 0;
        try {
            Scanner scanner = new Scanner(f);
            while (scanner.hasNextLine()) {
                count++;
                scanner.nextLine();
            }
        } catch (FileNotFoundException e) {

        }
        return count;
    }
}

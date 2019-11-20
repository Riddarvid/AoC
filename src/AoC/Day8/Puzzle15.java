package AoC.Day8;

import AoC.FileUtilities;
import java.io.File;

public class Puzzle15 {
    public static void main(String[] args) {
        new Puzzle15().program();
    }

    private void program() {
        Integer[] input = readFromFile();
        Tree tree = new Tree();
        tree.fillTree(input);
        System.out.println(tree.getValue());
    }


    private Integer[] readFromFile() {
        String input = FileUtilities.getArrayFromFile(new File("src/AoC/Day8/Input.txt"))[0];
        String[] arr = input.split(" ");
        Integer[] indata = new Integer[arr.length];
        for (int i = 0; i < arr.length; i++) {
            indata[i] = Integer.parseInt(arr[i]);
        }
        return indata;
    }
}

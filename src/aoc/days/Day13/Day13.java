package aoc.days.Day13;

import aoc.FileUtilities;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Day13 {
    char[][] map;
    List<Cart> carts = new ArrayList<>();

    public Day13() {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day13/input.txt"));
        map = new char[input.length][];
        for (int i = 0; i < input.length; i++) {
            map[i] = input[i].toCharArray();
        }
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                switch (map[row][col]) {
                    case '^':
                        carts.add(new Cart(col, row, AbsoluteDirection.NORTH, map));
                        map[row][col] = '|';
                        break;
                    case '>':
                        carts.add(new Cart(col, row, AbsoluteDirection.EAST, map));
                        map[row][col] = '-';
                        break;
                    case 'v':
                        carts.add(new Cart(col, row, AbsoluteDirection.SOUTH, map));
                        map[row][col] = '|';
                        break;
                    case '<':
                        carts.add(new Cart(col, row, AbsoluteDirection.WEST, map));
                        map[row][col] = '-';
                }
            }
        }
        Cart.setCarts(carts);
    }

    public void update() {
        for (Cart c : carts) {
            c.update();
        }
        List<Cart> toBeRemoved = new ArrayList<>();
        for (Cart c : carts) {
            if (c.isToBeRemoved()) {
                toBeRemoved.add(c);
            }
        }
        for (Cart c: toBeRemoved) {
            carts.remove(c);
        }
    }

    public static void printMap(char[][] map) {
        for (char[] row : map) {
            //System.out.println(Arrays.toString(row));
            for (char c : row) {
                System.out.print(c);
            }
            System.out.println();
        }
    }
}

package aoc.days.Day18;

import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Day18 {
    char[][] map;
    List<State> states = new ArrayList<>();

    public static void main(String[] args) {
        new Day18();
    }

    public Day18() {
        String input[] = FileUtilities.getArrayFromFile(new File("src/AoC/Day18/input.txt"));
        map = RegEx.getCharArr(input);
        states.add(new State(map, 0));
        LumberView view = new LumberView(this, 20);
        int i = 1;
        for (; i <= 1000000000; i++) {
            map = step(map);
            State state = new State(map, i);
            if (states.contains(state)) {
                state = states.get(states.indexOf(state));
                state.iterations.add(i);
                int lengthOfCycle = state.iterations.get(1) - state.iterations.get(0);
                System.out.println("Length of cycle = " + lengthOfCycle);
                int startOfCycle = state.iterations.get(0);
                System.out.println("Start of cycle = " + startOfCycle);
                int numberOfCyclesLeft = (1000000000 - startOfCycle) % lengthOfCycle;
                System.out.println("Number of cycles until answer = " + numberOfCyclesLeft);
                i = 1000000000 - numberOfCyclesLeft;
            } else {
                states.add(state);
            }
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {

            }
            view.update();
        }
        System.out.println(count('#', map) * count('|', map));
    }

    private char[][] step(char[][] map) {
        char[][] newMap = new char[map.length][map[0].length];
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == '.') {
                    if (numberOfAdjacent('|', row, col, map) >= 3) {
                        newMap[row][col] = '|';
                    } else {
                        newMap[row][col] = '.';
                    }
                } else if (map[row][col] == '|') {
                    if (numberOfAdjacent('#', row, col, map) >= 3) {
                        newMap[row][col] = '#';
                    } else {
                        newMap[row][col] = '|';
                    }
                } else {
                    if (numberOfAdjacent('#', row, col, map) >= 1 && numberOfAdjacent('|', row, col, map) >= 1) {
                        newMap[row][col] = '#';
                    } else {
                        newMap[row][col] = '.';
                    }
                }
            }
        }
        return newMap;
    }

    private int numberOfAdjacent(char character, int row, int col, char[][] map) {
        int sum = 0;
        int r = row - 1;
        int maxRow = row + 1;
        int maxCol = col + 1;
        if (row == map.length - 1) {
            maxRow = row;
        }
        if (col == map[0].length - 1) {
            maxCol = col;
        }
        if (r < 0) {
            r = 0;
        }
        for (; r <= maxRow; r++) {
            int c = col - 1;
            if (c < 0) {
                c = 0;
            }
            for (; c <= maxCol; c++) {
                if (r >= 0 && c >= 0 && !(r == row && c == col)) {
                    if (map[r][c] == character) {
                        sum++;
                    }
                }
            }
        }
        return sum;
    }

    private int count(char character, char[][] map) {
        int sum = 0;
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == character) {
                    sum++;
                }
            }
        }
        return sum;
    }
}

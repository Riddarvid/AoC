package AoC.Day17;

import AoC.FileUtilities;
import AoC.RegEx;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

public class Day17 {
    char[][] map;
    int oldNumberOfWaterBlocks = -1;
    int minY;
    int maxY;
    WaterView view;
    List<Water> waterList;

    boolean[] rowsToBeEvaluated;

    public static void main(String[] args) {
        new Day17();
    }

    public Day17() {
        rowsToBeEvaluated = new boolean[1700];
        map = new char[1700][1700];
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                map[row][col] = '.';
            }
        }
        String input[] = FileUtilities.getArrayFromFile(new File("src/AoC/Day17/Input.txt"));
        for (String s : input) {
            List<Integer> ints = RegEx.getIntegers(s);
            int x;
            int y;
            if (s.charAt(0) == 'x') {
                x = ints.get(0);
                y = ints.get(1);
                for (; y <= ints.get(2); y++) {
                    map[y][x] = '#';
                }
            } else {
                y = ints.get(0);
                x = ints.get(1);
                for (; x <= ints.get(2); x++) {
                    map[y][x] = '#';
                }
            }
        }
        minY = getMiny();
        maxY = getMaxY();
        waterList = new ArrayList<>();
        waterList.add(new Water(500, 0));
        map[0][500] = '|';
        view = new WaterView(this, 1800, 1000);
        while (oldNumberOfWaterBlocks != waterList.size()) {
            try {
                Thread.sleep(5);
            } catch (InterruptedException e){

            }
            oldNumberOfWaterBlocks = waterList.size();
            step();
        }
        System.out.println("Total water:" + numberOfValidBlocks());
        System.out.println("Still water:" + numberOfValidStillBlocks());
        createFile();
        //System.out.println(getNumberOfReachable(500, 0));
    }

    private int numberOfValidStillBlocks() {
        int sum = 0;
        for (Water w : waterList) {
            if (w.y <= maxY && w.y >= minY && w.still) {
                sum++;
            }
        }
        return sum;
    }

    private void step() {
        List<Water> toBeAdded = new ArrayList<>();
        for (Water w : waterList) {
            w.flow(map, toBeAdded);
        }
        waterList.addAll(toBeAdded);
        for (Water w : waterList) {
            w.makeStill(map, waterList);
        }
        view.update();
    }

    private int numberOfWaterBlocks() {
        int sum = 0;
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == '~' || map[row][col] == '|') {
                    sum++;
                }
            }
        }
        return sum;
    }

    private int numberOfValidBlocks() {
        int sum = 0;
        /*for (int row = minY; row <= maxY; row++) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == '~' || map[row][col] == '|') {
                    sum++;
                }
            }
        }*/
        for (Water w : waterList) {
            if (w.y <= maxY && w.y >= minY) {
                sum++;
            }
        }
        return sum;
    }

    private void reevaluate() {
        for (int row = 0; row < rowsToBeEvaluated.length; row++) {
            if (rowsToBeEvaluated[row]) {
                for (int col = 0; col < map[row].length; col++) {
                    if (map[row][col] == '|' && (map[row + 1][col] == '#' || map[row + 1][col] == '~')) {
                        placeLeft(col - 1, row);
                        placeRight(col + 1, row);
                    }
                }
            }
        }
        rowsToBeEvaluated = new boolean[1700];
    }

    private int getMaxY() {
        for (int row = map.length - 1; row >= 0; row--) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == '#') {
                    return row;
                }
            }
        }
        return 0;
    }

    private int getMiny() {
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == '#') {
                    return row;
                }
            }
        }
        return 0;
    }

    private void makeFlowingStill() {
        for (int row = 0; row < map.length; row++) {
            makeFlowingStill(row);
        }
    }

    private void makeFlowingStill(int row) {
        for (int col = 0; col < map[row].length; col++) {
            if (map[row][col] == '#') {
                int startX = col + 1;
                if (shouldFill(startX, row)) {
                    rowsToBeEvaluated[row - 1] = true;
                    int endX = getFillRange(startX, row);
                    for (;startX < endX; startX++) {
                        map[row][startX] = '~';
                    }
                }
            }
        }
    }

    private boolean shouldFill(int startX, int row) {
        int x = startX;
        while (map[row][x] == '|') {
            if (map[row + 1][x] != '#' && map[row + 1][x] != '~') {
                return false;
            }
            x++;
        }
        if (map[row][x] == '#') {
            return true;
        }
        return false;
    }

    private int getFillRange(int startX, int row) {
        int x = startX;
        while (map[row][x] == '|') {
            if (map[row + 1][x] != '#' && map[row + 1][x] != '~') {
                return startX;
            }
            x++;
        }
        if (map[row][x] == '#') {
            return x;
        }
        return startX;
    }

    private int getNumberOfWaterBlocks() {
        int count = 0;
        for (int row = 0; row < map.length; row++) {
            for (int col = 0; col < map[row].length; col++) {
                if (map[row][col] == '|' || map[row][col] == '~') {
                    count++;
                }
            }
        }
        return count;
    }

    private static void printMatrix(char[][] matrix) {
        for (int row = 0; row < matrix.length; row++) {
            for (int col = 0; col < matrix[row].length; col++) {
                System.out.print(matrix[row][col]);
            }
            System.out.println();
        }
    }

    private void placeWater(int x, int y) {
        map[y][x] = '|';
        if (y < maxY) {
            if (map[y + 1][x] == '.' || map[y + 1][x] == '|') {
                placeWater(x, y + 1);
            } else if (map[y + 1][x] != '|') {
                placeRight(x + 1, y);
                placeLeft(x - 1, y);
            }
        }
    }

    private void placeRight(int x, int y) {
        if (map[y][x] != '#' && map[y][x] != '~') {
            if (map[y + 1][x] == '#' || map[y + 1][x] == '~') {
                map[y][x] = '|';
                placeRight(x + 1, y);
            } else {
                placeWater(x, y);
            }
        }
    }

    private void placeLeft(int x, int y) {
        if (map[y][x] != '#' && map[y][x] != '~') {
            if (map[y + 1][x] == '#' || map[y + 1][x] == '~') {
                map[y][x] = '|';
                placeLeft(x - 1, y);
            } else {
                placeWater(x, y);
            }
        }
    }

    private void createFile() {
        try {
            PrintWriter writer = new PrintWriter("src\\AoC\\Day17\\Outdata", "UTF-8");
            for (int row = 0; row < map.length; row++) {
                for (int col = 400; col < 900; col++) {
                    writer.print(map[row][col]);
                }
                writer.println();
            }
            writer.close();
        } catch (FileNotFoundException e) {

        } catch (UnsupportedEncodingException e) {

        }

    }
}

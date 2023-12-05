package aoc.days.day8;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.output.OutputUtils;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day8 extends Day {
    private boolean[][] screen;
    private int width;
    private int height;
    private List<Instruction> instructions;

    public static void main(String[] args) {
        new Day8();
    }

    @Override
    protected void part1() {
        for (Instruction instruction : instructions) {
            int a = instruction.getA();
            int b = instruction.getB();
            switch (instruction.getType()) {
                case RECT:
                    rect(a, b);
                    break;
                case ROTROW:
                    rotateRow(a, b);
                    break;
                case ROTCOL:
                    rotateColumn(a, b);
                    break;
                default:
                    throw new InputMismatchException();
            }
        }
        System.out.println(count(screen));
    }

    private int count(boolean[][] matrix) {
        int sum = 0;
        for (boolean[] row : matrix) {
            sum += count(row);
        }
        return sum;
    }

    private int count(boolean[] row) {
        int sum = 0;
        for (boolean element : row) {
            if (element) {
                sum++;
            }
        }
        return sum;
    }

    private void rotateColumn(int a, int b) {
        boolean[] newColumn = new boolean[height];
        for (int i = 0; i < height; i++) {
            newColumn[(i + b) % height] = screen[i][a];
        }
        for (int i = 0; i < height; i++) {
            screen[i][a] = newColumn[i];
        }
    }

    private void rotateRow(int a, int b) {
        boolean[] oldRow = screen[a];
        boolean[] newRow = new boolean[width];
        for (int i = 0; i < width; i++) {
            newRow[(i + b) % width] = oldRow[i];
        }
        screen[a] = newRow;
    }

    private void rect(int a, int b) {
        for (int y = 0; y < b; y++) {
            for (int x = 0; x < a; x++) {
                screen[y][x] = true;
            }
        }
    }

    @Override
    protected void part2() {
        OutputUtils.printMatrix(screen);
    }

    @Override
    protected void setup() {
        width = 50;
        height = 6;
        screen = new boolean[height][width];
        instructions = new ArrayList<>();
        for (String s : lines) {
            List<Integer> values = InputUtils.getInts(s);
            Type type;
            if (s.charAt(1) == 'e') {
                type = Type.RECT;
            } else if (s.charAt(7) == 'r') {
                type = Type.ROTROW;
            } else {
                type = Type.ROTCOL;
            }
            instructions.add(new Instruction(type, values.get(0), values.get(1)));
        }
    }
}

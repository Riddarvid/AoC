package aoc21.days.day4;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day4 extends Day {
    private List<Board> boards1;
    private List<Board> boards2;
    private List<Integer> numbersToDraw;

    private void printBoards(List<Board> boards) {
        for (Board board : boards) {
            System.out.println(board.toString());
            System.out.println();
        }
    }

    private void printBoardsCompact(List<Board> boards) {
        boards = new ArrayList<>(boards);
        int boardsPerRow = 11;
        int boardIndex = 0;
        while (boardIndex < boards.size()) {
            String[] currentRows = new String[5];
            for (int i = 0; i < 5; i++) {
                currentRows[i] = "";
            }
            for (int i = 0; i < boardsPerRow && boardIndex + i < boards.size(); i++) {
                Board board = boards.get(boardIndex + i);
                for (int row = 0; row < 5; row++) {
                    currentRows[row] += " " + board.getRowString(row);
                }
            }
            for (String toPrint : currentRows) {
                System.out.println(toPrint);
            }
            System.out.println();
            boardIndex += boardsPerRow;
        }
    }

    private int playToWin(List<Board> boards, int drawIndex, boolean debug) {
        if (debug) {
            System.out.println("Initial boards:\n");
            printBoardsCompact(boards);
        }
        while (drawIndex < numbersToDraw.size()) {
            int draw = numbersToDraw.get(drawIndex);
            if (debug) {
                System.out.println("Number called: " + draw + "\n");
            }
            for (Board board : boards) {
                board.mark(draw);
                if (board.hasBingo()) {
                    int score = board.getSumOfUnmarked();
                    return score * draw;
                }
            }
            if (debug) {
                System.out.println("Boards after marking:\n");
                printBoardsCompact(boards);
            }
            drawIndex++;
        }
        throw new IllegalArgumentException("No bingo was achieved.");
    }

    private int playToLose(List<Board> boards) {
        System.out.println("Initial boards:\n");
        printBoardsCompact(boards);
        int drawIndex = 0;
        while (boards.size() > 1) {
            int draw = numbersToDraw.get(drawIndex);
            System.out.println("Number called: " + draw + "\n");
            drawIndex++;
            Set<Board> toRemove = new HashSet<>();
            for (Board board : boards) {
                board.mark(draw);
                if (board.hasBingo()) {
                    toRemove.add(board);
                }
            }
            System.out.println("Boards before removal:\n");
            printBoardsCompact(boards);
            boards.removeAll(toRemove);
        }
        return playToWin(boards, drawIndex, true);
    }

    public static void main(String[] args) {
        new Day4().runAndPrint();
    }

    public Day4() {
        super("input4");
    }

    @Override
    public long part1() {
        return playToWin(boards1, 0, false);
    }

    @Override
    public long part2() {
        return playToLose(boards2);
    }

    @Override
    public void setup() {
        boards1 = new ArrayList<>();
        boards2 = new ArrayList<>();
        numbersToDraw = ParsingUtils.getIntegers(lines.get(0));
        int i = 2;
        while (i < lines.size()) {
            List<List<Integer>> numbers = new ArrayList<>();
            for (int j = 0; j < 5; j++) {
                numbers.add(ParsingUtils.getIntegers(lines.get(i + j)));
            }
            boards1.add(new Board(numbers));
            boards2.add(new Board(numbers));
            i += 6;
        }
    }
}

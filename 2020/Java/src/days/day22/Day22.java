package days.day22;

import riddarvid.aoc.days.Day;

import java.util.LinkedList;

public class Day22 extends Day {
    private Game game1;
    private Game game2;

    public static void main(String[] args) {
        new Day22().runAndPrint();
    }

    @Override
    public long part1() {
        return game1.getWinningScore();
    }

    @Override
    public long part2() {
        return game2.getWinningScore();
    }

    @Override
    public void setup() {
        LinkedList<Integer> deckPlayer1 = parseDeck(1);
        int index = 3;
        while (!lines.get(index).equals("")) {
            index++;
        }
        LinkedList<Integer> deckPlayer2 = parseDeck(index + 2);
        game1 = new NormalGame(deckPlayer1, deckPlayer2);
        game2 = new RecursiveGame(deckPlayer1, deckPlayer2);
    }

    private LinkedList<Integer> parseDeck(int index) {
        LinkedList<Integer> deck = new LinkedList<>();
        while (index < lines.size() && !lines.get(index).equals("")) {
            deck.addLast(Integer.parseInt(lines.get(index)));
            index++;
        }
        return deck;
    }
}

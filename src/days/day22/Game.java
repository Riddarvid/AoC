package days.day22;

import java.util.Iterator;
import java.util.LinkedList;

public abstract class Game {
    protected final LinkedList<Integer> deckPlayer1;
    protected final LinkedList<Integer> deckPlayer2;
    protected int winner;
    protected boolean played = false;

    protected Game(LinkedList<Integer> deckPlayer1, LinkedList<Integer> deckPlayer2) {
        this.deckPlayer1 = new LinkedList<>(deckPlayer1);
        this.deckPlayer2 = new LinkedList<>(deckPlayer2);
    }

    protected Game(LinkedList<Integer> deckPlayer1, int n1, LinkedList<Integer> deckPlayer2, int n2) {
        this.deckPlayer1 = new LinkedList<>();
        for (int i = 0; i < n1; i++) {
            this.deckPlayer1.add(deckPlayer1.get(i));
        }
        this.deckPlayer2 = new LinkedList<>();
        for (int i = 0; i < n2; i++) {
            this.deckPlayer2.add(deckPlayer2.get(i));
        }
    }

    protected abstract void play();

    public int getWinner() {
        if (!played) {
            play();
        }
        return winner;
    }

    public long getWinningScore() {
        if (!played) {
            play();
        }
        LinkedList<Integer> deck;
        if (winner == 1) {
            deck = deckPlayer1;
        } else {
            deck = deckPlayer2;
        }
        return calculateScore(deck);
    }

    private long calculateScore(LinkedList<Integer> deck) {
        int factor = 1;
        int score = 0;
        Iterator<Integer> iterator = deck.descendingIterator();
        while (iterator.hasNext()) {
            score += factor * iterator.next();
            factor++;
        }
        return score;
    }
}

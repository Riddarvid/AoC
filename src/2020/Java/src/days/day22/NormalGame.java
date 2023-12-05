package days.day22;

import java.util.LinkedList;

public class NormalGame extends Game {
    public NormalGame(LinkedList<Integer> deckPlayer1, LinkedList<Integer> deckPlayer2) {
        super(deckPlayer1, deckPlayer2);
    }

    protected void play() {
        while (!deckPlayer1.isEmpty() && !deckPlayer2.isEmpty()) {
            int cardPlayer1 = deckPlayer1.removeFirst();
            int cardPlayer2 = deckPlayer2.removeFirst();
            if (cardPlayer1 > cardPlayer2) {
                deckPlayer1.addLast(cardPlayer1);
                deckPlayer1.addLast(cardPlayer2);
            } else {
                deckPlayer2.addLast(cardPlayer2);
                deckPlayer2.addLast(cardPlayer1);
            }
        }
        played = true;
        if (deckPlayer1.isEmpty()) {
            winner = 2;
        } else {
            winner = 1;
        }
    }
}

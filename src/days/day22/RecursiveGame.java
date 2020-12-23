package days.day22;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

public class RecursiveGame extends Game {
    private final Set<State> states;

    protected RecursiveGame(LinkedList<Integer> deckPlayer1, LinkedList<Integer> deckPlayer2) {
        super(deckPlayer1, deckPlayer2);
        states = new HashSet<>();
    }

    public RecursiveGame(LinkedList<Integer> deckPlayer1, int n1, LinkedList<Integer> deckPlayer2, int n2) {
        super(deckPlayer1, n1, deckPlayer2, n2);
        states = new HashSet<>();
    }

    @Override
    protected void play() {
        boolean done = false;
        while (!done && !deckPlayer1.isEmpty() && !deckPlayer2.isEmpty()) {
            State state = new State(deckPlayer1, deckPlayer2);
            if (states.contains(state)) {
                winner = 1;
                done = true;
            } else {
                states.add(state);
                int cardPlayer1 = deckPlayer1.removeFirst();
                int cardPlayer2 = deckPlayer2.removeFirst();
                int roundWinner;
                if (deckPlayer1.size() >= cardPlayer1 && deckPlayer2.size() >= cardPlayer2) {
                    Game recursive = new RecursiveGame(deckPlayer1, cardPlayer1, deckPlayer2, cardPlayer2);
                    roundWinner = recursive.getWinner();
                } else {
                    if (cardPlayer1 > cardPlayer2) {
                        roundWinner = 1;
                    } else {
                        roundWinner = 2;
                    }
                }
                if (roundWinner == 1) {
                    deckPlayer1.addLast(cardPlayer1);
                    deckPlayer1.addLast(cardPlayer2);
                } else {
                    deckPlayer2.addLast(cardPlayer2);
                    deckPlayer2.addLast(cardPlayer1);
                }
            }
        }
        played = true;
        if (winner == 0) {
            if (deckPlayer1.isEmpty()) {
                winner = 2;
            } else {
                winner = 1;
            }
        }
    }
}

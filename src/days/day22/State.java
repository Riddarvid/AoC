package days.day22;

import java.util.LinkedList;
import java.util.Objects;

public class State {
    private final LinkedList<Integer> deckPlayer1;
    private final LinkedList<Integer> deckPlayer2;

    public State(LinkedList<Integer> deckPlayer1, LinkedList<Integer> deckPlayer2) {
        this.deckPlayer1 = new LinkedList<>(deckPlayer1);
        this.deckPlayer2 = new LinkedList<>(deckPlayer2);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        State state = (State) o;
        return Objects.equals(deckPlayer1, state.deckPlayer1) && Objects.equals(deckPlayer2, state.deckPlayer2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(deckPlayer1, deckPlayer2);
    }
}

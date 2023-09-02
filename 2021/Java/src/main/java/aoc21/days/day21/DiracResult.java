package aoc21.days.day21;

public class DiracResult {
    private long playerOneWins;
    private long playerTwoWins;

    public DiracResult() {
        playerOneWins = 0;
        playerTwoWins = 0;
    }

    public void addPlayerOneWins(long combinations) {
        playerOneWins += combinations;
    }

    public void addPlayerTwoWins(long combinations) {
        playerTwoWins += combinations;
    }

    public long getPlayerOneWins() {
        return playerOneWins;
    }

    public long getPlayerTwoWins() {
        return playerTwoWins;
    }
}

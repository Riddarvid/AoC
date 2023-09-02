package aoc21.days.day21;

public class DiracGame {
    private final int startPosition1;
    private final int startPosition2;

    private DiracResult result;

    private final int scoreToWin;

    public DiracGame(int startPosition1, int startPosition2, int scoreToWin) {
        this.startPosition1 = startPosition1;
        this.startPosition2 = startPosition2;
        this.scoreToWin = scoreToWin;
    }

    public void play() {
        DiracResult result = new DiracResult();
        play(startPosition1, startPosition2, 0, 0, 1, true, result);
        this.result = result;
    }

    public void play(int position1, int position2, int score1, int score2, long combinations, boolean playerOneTurn, DiracResult result) {
        if (score1 >= scoreToWin) {
            result.addPlayerOneWins(combinations);
            return;
        }
        if (score2 >= scoreToWin) {
            result.addPlayerTwoWins(combinations);
            return;
        }
        for (int roll = 3; roll <= 9; roll++) {
            long newCombinations = combinations;
            switch (roll) {
                case 3, 9 -> newCombinations *= 1;
                case 4, 8 -> newCombinations *= 3;
                case 5, 7 -> newCombinations *= 6;
                case 6 -> newCombinations *= 7;
                default -> throw new IllegalArgumentException();
            }
            int newPosition1 = position1;
            int newScore1 = score1;
            int newPosition2 = position2;
            int newScore2 = score2;
            if (playerOneTurn) {
                newPosition1 = (newPosition1 - 1 + roll) % 10 + 1;
                newScore1 = newScore1 + newPosition1;
            } else {
                newPosition2 = (newPosition2 - 1 + roll) % 10 + 1;
                newScore2 = newScore2 + newPosition2;
            }
            play(newPosition1, newPosition2, newScore1, newScore2, newCombinations, !playerOneTurn, result);
        }
    }

    public DiracResult getResult() {
        return result;
    }
}

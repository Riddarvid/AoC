package aoc21.days.day21;

public class Game {
    private int position1;
    private int position2;

    private int score1;
    private int score2;

    private final Dice dice;

    public Game(int position1, int position2, Dice dice) {
        this.position1 = position1;
        this.position2 = position2;
        score1 = 0;
        score2 = 0;
        this.dice = dice;
    }

    public void play() {
        boolean playerOneTurn = true;
        while (score1 < 1000 && score2 < 1000) {
            playRound(playerOneTurn);
            playerOneTurn = !playerOneTurn;
        }
    }

    private void playRound(boolean playerOneTurn) {
        int roll1 = dice.roll();
        int roll2 = dice.roll();
        int roll3 = dice.roll();
        if (playerOneTurn) {
            position1 = (position1 - 1 + roll1 + roll2 + roll3) % 10 + 1;
            score1 += position1;
        } else {
            position2 = (position2 - 1 + roll1 + roll2 + roll3) % 10 + 1;
            score2 += position2;
        }
    }

    public int getScore1() {
        return score1;
    }

    public int getScore2() {
        return score2;
    }
}

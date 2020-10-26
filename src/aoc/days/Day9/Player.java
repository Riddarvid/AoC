package aoc.days.Day9;

public class Player {
    private long score = 0;

    public void addScore(int points) {
        score += points;
    }

    public long getScore() {
        return score;
    }

    @Override
    public String toString() {
        return "score=" + score;
    }
}

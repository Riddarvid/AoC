package AoC.Day9;

import java.util.ArrayList;
import java.util.List;

public class Day9 {

    public static long game(int numberOfPlayers, int totalNumberOfMarbles) {
        Player[] players = new Player[numberOfPlayers];
        for (int i = 0; i < numberOfPlayers; i++) {
            players[i] = new Player();
        }
        Circle circle = new Circle();
        for (int i = 1; i < totalNumberOfMarbles; i++) {
            circle.addMarble(i, players[i % numberOfPlayers]);
        }
        return maxScore(players);
    }

    private static long maxScore(Player[] players) {
        long max = players[0].getScore();
        for (Player p : players) {
            if (p.getScore() > max) {
                max = p.getScore();
            }
        }
        return max;
    }
}

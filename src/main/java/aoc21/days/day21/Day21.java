package aoc21.days.day21;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

public class Day21 extends Day {
    private int startingPosition1;
    private int startingPosition2;

    @Override
    public void setup() {
        startingPosition1 = ParsingUtils.getIntegers(lines.get(0)).get(1);
        startingPosition2 = ParsingUtils.getIntegers(lines.get(1)).get(1);
    }

    @Override
    public long part1() {
        Dice dice = new DeterministicDice();
        DeterministicGame game = new DeterministicGame(startingPosition1, startingPosition2, 1000, dice);
        game.play();
        long losingScore = Math.min(game.getScore1(), game.getScore2());
        long numberOfRolls = dice.getNumberOfRolls();
        return losingScore * numberOfRolls;
    }

    @Override
    public long part2() {
        DiracGame game = new DiracGame(startingPosition1, startingPosition2, 21);
        game.play();
        DiracResult result = game.getResult();
        return Math.max(result.getPlayerOneWins(), result.getPlayerTwoWins());
    }
}

package aoc15.days.day22;

import aoc15.days.day22.actors.Boss;
import aoc15.days.day22.actors.Player;
import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.PriorityQueue;
import java.util.Queue;

public class Day22 extends Day {
    private int bossHP;
    private int bossDamage;

    @Override
    public long part1() {
        Player player = new Player(50, 500, 0);
        Boss boss = new Boss(bossHP, bossDamage);
        Queue<State> stateQueue = new PriorityQueue<>(new StateComparator());
        stateQueue.add(new State(player, boss, false));
        int leastManaSpent = 0;
        while (!stateQueue.isEmpty()) {
            State current = stateQueue.poll();
            if (current.isFinished()) {
                if (current.playerWon()) {
                    leastManaSpent = current.getManaSpent();
                    break;
                }
            } else {
                stateQueue.addAll(current.generateNextStates());
            }
        }
        return leastManaSpent;
    }

    @Override
    public long part2() {
        Player player = new Player(50, 500, 0);
        Boss boss = new Boss(bossHP, bossDamage);
        Queue<State> stateQueue = new PriorityQueue<>(new StateComparator());
        stateQueue.add(new State(player, boss, true));
        int leastManaSpent = 0;
        while (!stateQueue.isEmpty()) {
            State current = stateQueue.poll();
            if (current.isFinished()) {
                if (current.playerWon()) {
                    leastManaSpent = current.getManaSpent();
                    break;
                }
            } else {
                stateQueue.addAll(current.generateNextStates());
            }
        }
        return leastManaSpent;
    }

    @Override
    public void setup() {
        bossHP = ParsingUtils.getIntegers(lines.get(0)).get(0);
        bossDamage = ParsingUtils.getIntegers(lines.get(1)).get(0);
    }
}

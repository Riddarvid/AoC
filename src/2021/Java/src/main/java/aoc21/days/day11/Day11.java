package aoc21.days.day11;

import aoc.days.Day;

public class Day11 extends Day {
    private Octopus[][] octopuses1;
    private Octopus[][] octopuses2;

    @Override
    public void setup() {
        octopuses1 = new Octopus[lines.size()][];
        octopuses2 = new Octopus[lines.size()][];
        for (int row = 0; row < lines.size(); row++) {
            String line = lines.get(row);
            Octopus[] octopusLine1 = new Octopus[line.length()];
            Octopus[] octopusLine2 = new Octopus[line.length()];
            for (int column = 0; column < line.length(); column++) {
                octopusLine1[column] = new Octopus(line.charAt(column) - '0');
                octopusLine2[column] = new Octopus(line.charAt(column) - '0');
            }
            octopuses1[row] = octopusLine1;
            octopuses2[row] = octopusLine2;
        }
    }

    //Returns the number of flashes that occurred during the step.
    private int step(Octopus[][] octopuses) {
        incrementAll(octopuses);
        executeFlashes(octopuses);
        int flashes = countFlashes(octopuses);
        resetFlashed(octopuses);
        return flashes;
    }

    private void incrementAll(Octopus[][] octopuses) {
        for (Octopus[] octopusLine : octopuses) {
            for (Octopus octopus : octopusLine) {
                octopus.incrementEnergyLevel();
            }
        }
    }

    private void executeFlashes(Octopus[][] octopuses) {
        boolean anyFlashed = true;
        while (anyFlashed) {
            anyFlashed = false;
            for (int row = 0; row < octopuses.length; row++) {
                for (int column = 0; column < octopuses[row].length; column++) {
                    Octopus octopus = octopuses[row][column];
                    if (!octopus.hasFlashed() && octopus.getEnergyLevel() > 9) {
                        octopus.flash();
                        incrementNeighbours(octopuses, row, column);
                        anyFlashed = true;
                    }
                }
            }
        }
    }

    private void incrementNeighbours(Octopus[][] octopuses, int row, int column) {
        for (int y = -1; y <= 1; y++) {
            for (int x = -1; x <= 1; x++) {
                int targetRow = row + y;
                int targetColumn = column + x;
                if (targetColumn < 0 || targetColumn >= octopuses[0].length || targetRow < 0 || targetRow >= octopuses.length) {
                    continue;
                }
                if (!(x == 0 && y == 0)) {
                    octopuses[row + y][column + x].incrementEnergyLevel();
                }
            }
        }
    }

    private int countFlashes(Octopus[][] octopuses) {
        int count = 0;
        for (Octopus[] octopusLine : octopuses) {
            for (Octopus octopus : octopusLine) {
                if (octopus.hasFlashed()) {
                    count++;
                }
            }
        }
        return count;
    }

    private void resetFlashed(Octopus[][] octopuses) {
        for (Octopus[] octopusLine : octopuses) {
            for (Octopus octopus : octopusLine) {
                if (octopus.hasFlashed()) {
                    octopus.reset();
                }
            }
        }
    }

    @Override
    public long part1() {
        long sum = 0;
        for (int i = 0; i < 100; i++) {
            sum += step(octopuses1);
        }
        return sum;
    }

    private boolean areSynchronized(Octopus[][] octopuses) {
        for (Octopus[] octopusLine : octopuses) {
            for (Octopus octopus : octopusLine) {
                if (octopus.getEnergyLevel() != 0) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public long part2() {
        int turns = 0;
        while (!areSynchronized(octopuses2)) {
            turns++;
            step(octopuses2);
        }
        return turns;
    }
}

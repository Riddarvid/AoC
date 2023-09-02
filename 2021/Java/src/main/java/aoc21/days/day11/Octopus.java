package aoc21.days.day11;

public class Octopus {
    private int energyLevel;
    private boolean hasFlashed;

    public Octopus(int energyLevel) {
        this.energyLevel = energyLevel;
        hasFlashed = false;
    }

    public int getEnergyLevel() {
        return energyLevel;
    }

    public void incrementEnergyLevel() {
        energyLevel++;
    }

    public boolean hasFlashed() {
        return hasFlashed;
    }

    public void flash() {
        hasFlashed = true;
    }

    public void reset() {
        hasFlashed = false;
        energyLevel = 0;
    }
}

package aoc.days.day19;

public class Elf {
    private final int id;
    private int presents;


    public Elf(int id) {
        this.id = id;
        presents = 1;
    }

    public void addPresents(int n) {
        presents += n;
    }

    public int getPresents() {
        return presents;
    }
}

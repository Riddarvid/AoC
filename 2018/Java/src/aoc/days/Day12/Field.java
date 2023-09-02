package aoc.days.Day12;

public class Field {
    private boolean[] rules;
    private boolean[] plants;
    private int startSize;

    public Field(boolean[] initialState, boolean[] rules) {
        this.rules = rules;
        this.plants = initialState;
        startSize = initialState.length;
        plants = new boolean[10000];
        for (int i = 0; i < initialState.length; i++) {
            plants[i + 100] = initialState[i];
        }
    }

    public void update() {
        boolean[] nextPlants = new boolean[plants.length];
        int index;
        for (int i = 2; i < plants.length - 2; i++) {
            index = getIndex(plants[i - 2], plants[i - 1], plants[i], plants[i + 1], plants[i + 2]);
            nextPlants[i] = rules[index];
        }
        plants = nextPlants;
    }

    public int getNumberOfPlants() {
        int counter = 0;
        for (boolean plant : plants) {
            if (plant) {
                counter++;
            }
        }
        return counter;
    }

    public int getSumOfPlants() {
        int sum = 0;
        for (int i = 0; i < plants.length; i++) {
            if (plants[i]) {
                sum += i - startSize;
            }
        }
        return sum;
    }

    public int getDiffHighestLowest() {
        int lowest = 0;
        int highest = 0;
        int i = 0;
        while (!plants[i]) {
            i++;
        }
        lowest = i;
        i = plants.length - 1;
        while (!plants[i]) {
            i--;
        }
        highest = i;
        return highest - lowest;
    }

    private int getIndex(boolean a, boolean b, boolean c, boolean d, boolean e) {
        int sum = 0;
        if (a) {
            sum += 16;
        }
        if (b) {
            sum += 8;
        }
        if (c) {
            sum += 4;
        }
        if (d) {
            sum += 2;
        }
        if (e) {
            sum += 1;
        }
        return sum;
    }

    public boolean[] getPlants() {
        return plants;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < plants.length; i++) {
            if (plants[i] == true) {
                sb.append('#');
            } else {
                sb.append('.');
            }
        }
        return sb.toString();
    }
}

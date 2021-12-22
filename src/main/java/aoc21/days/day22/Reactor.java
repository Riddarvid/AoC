package aoc21.days.day22;

import java.util.HashSet;
import java.util.Set;

public class Reactor {
    private final Set<Cuboid> positiveCuboids;
    private final Set<Cuboid> negativeCuboids;

    public Reactor() {
        positiveCuboids = new HashSet<>();
        negativeCuboids = new HashSet<>();
    }

    public void executeInstruction(Instruction instruction) {
        Cuboid cuboid = instruction.getCuboid();
        Set<Cuboid> negativeToAdd = new HashSet<>();
        for (Cuboid positive : positiveCuboids) {
            if (positive.overlaps(cuboid)) {
                negativeToAdd.add(positive.getOverlap(cuboid));
            }
        }
        Set<Cuboid> positiveToAdd = new HashSet<>();
        for (Cuboid negative : negativeCuboids) {
            if (negative.overlaps(cuboid)) {
                positiveToAdd.add(negative.getOverlap(cuboid));
            }
        }
        if (instruction.getState() == Instruction.State.ON) {
            positiveCuboids.add(cuboid);
        }
        negativeCuboids.addAll(negativeToAdd);
        positiveCuboids.addAll(positiveToAdd);
    }

    public long getNumberOfLit() {
        long sum = 0;
        for (Cuboid positive : positiveCuboids) {
            sum += positive.size();
        }
        for (Cuboid negative : negativeCuboids) {
            sum -= negative.size();
        }
        return sum;
    }
}

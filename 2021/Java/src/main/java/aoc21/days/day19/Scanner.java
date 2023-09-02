package aoc21.days.day19;

import aoc.math.geometry.PointND;
import aoc.math.geometry.VectorND;

import java.util.*;

public class Scanner {
    private final List<PointND> originalPoints;
    private final Collection<List<PointND>> configurations;
    private final int id;

    private PointND position;
    private List<PointND> determinedBeacons; //IMPORTANT: Relative to origo, not to the scanner!

    public Scanner(List<PointND> originalPoints, int id) {
        this.originalPoints = new ArrayList<>(originalPoints);
        this.id = id;
        configurations = new ArrayList<>();
        generateConfigurations();
        //Utgå från att från början är vårt koordinatsystem (x,y,z)
        //Prova sedan med alla 6 riktningar som positiv x.
        //Rotera sedan runt denna axel för att få alla 4 möjligheter
        //Totalt 24 set av punkter
    }

    private void generateConfigurations() {
        //In the first position, swap with x, -x, y, -y, z, -z
        //Then generate rotations
        //TODO gör detta via basbyten istället
        for (int cord = 0; cord < 3; cord++) {
            generateRotations(changeDirection(originalPoints, cord, false));
            generateRotations(changeDirection(originalPoints, cord, true));
        }
    }

    private List<PointND> changeDirection(Collection<PointND> toSwap, int index, boolean invert) {
        List<PointND> swapped = new ArrayList<>(); //Choose between arraylist and set
        for (PointND point : toSwap) {
            int[] original = point.getCoordinates();
            if (index == 0) {
                if (invert) {
                    original[0] = -original[0];
                    original[1] = -original[1];//We must always invert another when we invert x
                }
            } else {
                int temp = original[index];
                if (invert) {
                    original[index] = -original[0];
                } else {
                    original[index] = original[0];
                }
                if (invert) {
                    original[0] = temp;
                } else {
                    original[0] = -temp;
                }
            }
            swapped.add(new PointND(original));
        }
        return swapped;
    }

    private void generateRotations(List<PointND> points) {
        //For the last two coordinates, try the combinations (1,1), (1,-1), (-1,1), (-1,-1)
        configurations.add(points);
        List<PointND> rotated = rotateAroundAxis(points);
        configurations.add(rotated);
        rotated = rotateAroundAxis(rotated);
        configurations.add(rotated);
        rotated = rotateAroundAxis(rotated);
        configurations.add(rotated);
    }

    private List<PointND> rotateAroundAxis(Collection<PointND> points) {
        List<PointND> rotated = new ArrayList<>();
        for (PointND point : points) {
            int x = point.getCoordinate(0);
            int y = point.getCoordinate(1);
            int z = point.getCoordinate(2);
            rotated.add(new PointND(x, -z, y));
        }
        return rotated;
    }

    public void setAsReferenceScanner() {
        position = new PointND(0, 0, 0);
        determinedBeacons = originalPoints;
    }

    public PointND getPosition() {
        return position;
    }

    public Collection<PointND> getDeterminedBeacons() {
        return new HashSet<>(determinedBeacons);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Scanner scanner = (Scanner) o;
        return Objects.equals(originalPoints, scanner.originalPoints);
    }

    @Override
    public int hashCode() {
        return Objects.hash(originalPoints);
    }

    public boolean tryOverlap(Scanner other) {
        for (List<PointND> otherConfiguration : other.configurations) {
            VectorND difference = tryMatch(otherConfiguration);
            if (difference != null) {
                other.determine(difference, otherConfiguration);
                return true;
            }
        }
        return false;
    }

    private VectorND tryMatch(List<PointND> otherConfiguration) {
        Map<VectorND, Integer> differenceCount = new HashMap<>();
        for (PointND point : determinedBeacons) {
            for (PointND other : otherConfiguration) {
                VectorND vector = other.vectorTo(point);
                int count = differenceCount.getOrDefault(vector, 0);
                differenceCount.put(vector, count + 1);
            }
        }
        VectorND mostCommonVector = null;
        int count = 0;
        for (VectorND vector : differenceCount.keySet()) {
            if (differenceCount.get(vector) > count) {
                mostCommonVector = vector;
                count = differenceCount.get(vector);
            }
        }
        if (count >= 12) {
            return mostCommonVector;
        }
        return null;
    }

    private void determine(VectorND difference, List<PointND> configuration) {
        position = PointND.getOrigo(3).moveBy(difference);
        determinedBeacons = new ArrayList<>();
        for (PointND point : configuration) {
            determinedBeacons.add(point.moveBy(difference));
        }
    }

    @Override
    public String toString() {
        return "ID: " + id;
    }
}

package aoc21.days.day23;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Corridor {
    private final static List<Integer> allowedPositions;

    static {
        allowedPositions = new ArrayList<>();
        allowedPositions.add(0);
        allowedPositions.add(1);
        allowedPositions.add(3);
        allowedPositions.add(5);
        allowedPositions.add(7);
        allowedPositions.add(9);
        allowedPositions.add(10);
    }

    private final List<Amphipod> positions;

    public Corridor(int n) {
        positions = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            positions.add(null);
        }
    }

    public Corridor(List<Amphipod> positions) {
        this.positions = new ArrayList<>(positions);
    }

    public List<Integer> getReachableRoomPositions(int startIndex) {
        List<Integer> indexes = new ArrayList<>();
        for (int i = startIndex - 1; i >= 0; i--) {
            if (positions.get(i) == null && allowedPositions.contains(i)) {
                indexes.add(i);
            } else {
                break;
            }
        }
        for (int i = startIndex + 1; i < positions.size(); i++) {
            if (positions.get(i) == null  && allowedPositions.contains(i)) {
                indexes.add(i);
            } else {
                break;
            }
        }
        return indexes;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Corridor corridor = (Corridor) o;
        return Objects.equals(positions, corridor.positions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(positions);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Amphipod position : positions) {
            if (position == null) {
                sb.append(".");
            } else {
                sb.append(position);
            }
        }
        return sb.toString();
    }

    public Amphipod get(int position) {
        return positions.get(position);
    }

    public int length() {
        return positions.size();
    }

    public boolean hasClearPath(int start, int end) { //Don't check start
        if (start < end) {
            for (int i = start + 1; i <= end; i++) {
                if (positions.get(i) != null) {
                    return false;
                }
            }
        } else {
            for (int i = start - 1; i >= end; i--) {
                if (positions.get(i) != null) {
                    return false;
                }
            }
        }
        return true;
    }

    public List<Integer> getAllowedPositions() {
        return allowedPositions;
    }

    public List<Amphipod> getPositions() {
        return new ArrayList<>(positions);
    }

    public Corridor copy() {
        return new Corridor(new ArrayList<>(positions));
    }
}

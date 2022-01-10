package aoc21.days.day23;

import java.util.Arrays;

public class Room {
    private final Character[] contents;

    public Room(Character[] contents) {
        this.contents = contents.clone();
    }

    public boolean canAccept(Character pod) {
        for (Character inhabitant : contents) {
            if (inhabitant != null && inhabitant != pod) {
                return false;
            }
        }
        return true;
    }

    public int distanceToInnermostFreeSpace() {
        int distance = 0;
        for (Character pod : contents) {
            if (pod != null) {
                return distance;
            }
            distance++;
        }
        return distance;
    }

    public int distanceToOutermostPod() {
        return distanceToInnermostFreeSpace() + 1;
    }

    public Character getOutermostPod() {
        for (Character pod : contents) {
            if (pod != null) {
                return pod;
            }
        }
        throw new IllegalArgumentException("No pod to return.");
    }

    public Room push(Character pod) {
        Character[] newContents = contents.clone();
        for (int i = 0; i < newContents.length; i++) {
            if (i == newContents.length - 1 || newContents[i + 1] != null) {
                newContents[i] = pod;
                return new Room(newContents);
            }
        }
        throw new IllegalArgumentException("No free space found?");
    }


    public Room pop() {
        Character[] newContents = contents.clone();
        for (int i = 0; i < newContents.length; i++) {
            if (newContents[i] != null) {
                newContents[i] = null;
                return new Room(newContents);
            }
        }
        throw new IllegalArgumentException("Can't pop from empty room.");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Room room = (Room) o;
        return Arrays.equals(contents, room.contents);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(contents);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("|");
        for (Character pod : contents) {
            if (pod == null) {
                sb.append('.');
            } else {
                sb.append(pod);
            }
        }
        return sb.toString();
    }
}

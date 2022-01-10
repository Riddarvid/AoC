package aoc21.days.day23;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class Burrow {
    private final Character[] corridor;
    private final static int[] allowedCorridorPositions = {0, 1, 3, 5, 7, 9, 10};
    private final Room[] rooms;

    public Burrow(Character[] corridor, Room[] rooms) {
        this.corridor = corridor;
        this.rooms = rooms;
    }

    public Set<Move> getMoves() {
        Set<Move> moves = new HashSet<>();
        moves.addAll(getCorridorToRoomMoves());
        if (moves.isEmpty()) {
            moves.addAll(getRoomsToCorridorMoves());
        }
        return moves;
    }

    private Set<Move> getRoomsToCorridorMoves() {
        Set<Move> moves = new HashSet<>();
        for (int i = 0; i < rooms.length; i++) {
            moves.addAll(getRoomToCorridorMoves(i));
        }
        return moves;
    }

    private Set<Move> getRoomToCorridorMoves(int roomIndex) {
        Set<Move> moves = new HashSet<>();
        Room room = rooms[roomIndex];
        if (room.canAccept((char) ('A' + roomIndex))) { //Room only contains correct type.
            return moves;
        }
        for (int targetPosition : allowedCorridorPositions) {
            int sourceCorridorPosition = roomIndex * 2 + 2;
            if (corridorIsClearBetween(sourceCorridorPosition, targetPosition)) {
                moves.add(getRoomToCorridorMove(roomIndex, sourceCorridorPosition, targetPosition));
            }
        }
        return moves;
    }

    private Move getRoomToCorridorMove(int roomIndex, int sourceCorridorPosition, int targetPosition) {
        Room room = rooms[roomIndex];
        int distance = Math.abs(targetPosition - sourceCorridorPosition);
        distance += room.distanceToOutermostPod();
        Character pod = room.getOutermostPod();
        int cost = (int) (distance * Math.pow(10, pod - 'A'));
        Character[] newCorridor = corridor.clone();
        newCorridor[targetPosition] = room.getOutermostPod();
        Room[] newRooms = rooms.clone();
        newRooms[roomIndex] = room.pop();
        return new Move(new Burrow(newCorridor, newRooms), cost);
    }

    private Set<Move> getCorridorToRoomMoves() {
        Set<Move> moves = new HashSet<>();
        for (int sourceCorridorPosition = 0; sourceCorridorPosition < corridor.length; sourceCorridorPosition++) {
            Character pod = corridor[sourceCorridorPosition];
            if (pod == null) {
                continue;
            }
            Room targetRoom = rooms[pod - 'A'];
            if (targetRoom.canAccept(pod)) {
                int targetCorridorPosition = (pod - 'A') * 2 + 2;
                if (corridorIsClearBetween(sourceCorridorPosition, targetCorridorPosition)) {
                    moves.add(getCorridorToRoomMove(sourceCorridorPosition, targetCorridorPosition, targetRoom, pod));
                }
            }
        }
        return moves;
    }

    private Move getCorridorToRoomMove(int sourceCorridorPosition, int targetCorridorPosition, Room targetRoom, Character pod) {
        int distance = Math.abs(sourceCorridorPosition - targetCorridorPosition);
        distance += targetRoom.distanceToInnermostFreeSpace();
        int cost = (int) (distance * Math.pow(10, pod - 'A'));
        Character[] newCorridor = corridor.clone();
        newCorridor[sourceCorridorPosition] = null;
        Room[] newRooms = rooms.clone();
        newRooms[pod - 'A'] = targetRoom.push(pod);
        return new Move(new Burrow(newCorridor, newRooms), cost);
    }

    private boolean corridorIsClearBetween(int sourceCorridorPosition, int targetCorridorPosition) {
        if (sourceCorridorPosition < targetCorridorPosition) {
            for (int i = sourceCorridorPosition + 1; i <= targetCorridorPosition; i++) {
                if (corridor[i] != null) {
                    return false;
                }
            }
        } else {
            for (int i = sourceCorridorPosition - 1; i >= targetCorridorPosition; i--) {
                if (corridor[i] != null) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Burrow burrow = (Burrow) o;
        return Arrays.equals(corridor, burrow.corridor) && Arrays.equals(rooms, burrow.rooms);
    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(corridor);
        result = 31 * result + Arrays.hashCode(rooms);
        return result;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Corridor: ");
        for (Character pod : corridor) {
            if (pod == null) {
                sb.append('.');
            } else {
                sb.append(pod);
            }
        }
        sb.append(' ');
        for (Room room : rooms) {
            sb.append(" ").append(room.toString());
        }
        return sb.toString();
    }
}

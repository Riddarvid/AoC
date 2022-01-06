package aoc21.days.day23;

import java.util.*;

public class Burrow {
    private final Corridor corridor;
    private final Map<Amphipod, Room> rooms;

    public Burrow(Corridor corridor, Map<Amphipod, Room> rooms) {
        this.corridor = corridor;
        this.rooms = rooms;
    }

    public Set<Move> getMoves() {
        Set<Move> moves = new HashSet<>();
        moves.addAll(getCorridorToRoomMoves());
        moves.addAll(getRoomsToCorridorMoves());
        return moves;
    }

    private Set<Move> getRoomsToCorridorMoves() {
        Set<Move> moves = new HashSet<>();
        for (Amphipod amphipod : rooms.keySet()) {
            moves.addAll(getRoomToCorridorMoves(amphipod));
        }
        return moves;
    }

    private Set<Move> getRoomToCorridorMoves(Amphipod roomType) {
        Set<Move> moves = new HashSet<>();
        Room room = rooms.get(roomType);
        if (room.getOuter() == null && room.getInner() == null) {
            return moves;
        }
        for (int targetPosition : corridor.getAllowedPositions()) {
            if (corridor.get(targetPosition) == null && corridor.hasClearPath(room.getPosition(), targetPosition)) {
                moves.add(moveRoomToCorridor(roomType, targetPosition));
            }
        }
        return moves;
    }

    private Set<Move> getCorridorToRoomMoves() {
        Set<Move> moves = new HashSet<>();
        for (int position = 0; position < corridor.length(); position++) {
            Amphipod amphipod = corridor.get(position);
            //If there is no amphipod here, continue
            if (amphipod == null) {
                continue;
            }
            Room targetRoom = rooms.get(amphipod);
            //If the amphipod is not allowed to enter the room, continue
            if (targetRoom.getOuter() != null) {//Room is full
                continue;
            }
            if (targetRoom.getInner() != null && !targetRoom.getInner().equals(amphipod)) {
                continue;
            }
            int targetPosition = rooms.get(amphipod).getPosition();
            //If the amphipod can't reach the room, continue.
            if (!corridor.hasClearPath(position, targetPosition)) {
                continue;
            }
            moves.add(moveCorridorToRoom(position, amphipod));
        }
        return moves;
    }

    private Move moveCorridorToRoom(int corridorPosition, Amphipod amphipod) {
        Room targetRoom = rooms.get(amphipod);
        int distance = Math.abs(corridorPosition - targetRoom.getPosition());
        if (targetRoom.getOuter() == null && targetRoom.getInner() == null) {
            distance += 2;
        } else if (targetRoom.getOuter() == null){
            distance += 1;
        } else {
            throw new IllegalArgumentException();
        }
        int cost = distance * (int)Math.pow(10, amphipod.ordinal());
        List<Amphipod> newPositions = corridor.getPositions();
        newPositions.remove(corridorPosition);
        newPositions.add(corridorPosition, null);
        Corridor newCorridor = new Corridor(newPositions);
        Map<Amphipod, Room> newRooms = new HashMap<>();
        for (Amphipod current : rooms.keySet()) {
            if (current.equals(amphipod)) {
                newRooms.put(current, rooms.get(current).push(amphipod));
            } else {
                newRooms.put(current, rooms.get(current).copy());
            }
        }
        Burrow newBurrow = new Burrow(newCorridor, newRooms);
        return new Move(newBurrow, cost);
    }

    private Move moveRoomToCorridor(Amphipod roomType, int corridorPosition) {
        Room sourceRoom = rooms.get(roomType);
        int distance = Math.abs(corridorPosition - sourceRoom.getPosition());
        Amphipod toMove;
        if (sourceRoom.getOuter() != null) {
            distance += 1;
            toMove = sourceRoom.getOuter();
        } else if (sourceRoom.getInner() != null){
            distance += 2;
            toMove = sourceRoom.getInner();
        } else {
            throw new IllegalArgumentException();
        }
        int cost = distance * (int)Math.pow(10, toMove.ordinal());
        List<Amphipod> newPositions = corridor.getPositions();
        newPositions.remove(corridorPosition);
        newPositions.add(corridorPosition, toMove);
        Corridor newCorridor = new Corridor(newPositions);
        Map<Amphipod, Room> newRooms = new HashMap<>();
        for (Amphipod current : rooms.keySet()) {
            if (current.equals(roomType)) {
                newRooms.put(current, rooms.get(current).pop());
            } else {
                newRooms.put(current, rooms.get(current).copy());
            }
        }
        Burrow newBurrow = new Burrow(newCorridor, newRooms);
        return new Move(newBurrow, cost);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Burrow burrow = (Burrow) o;
        return Objects.equals(corridor, burrow.corridor) && Objects.equals(rooms, burrow.rooms);
    }

    @Override
    public int hashCode() {
        return Objects.hash(corridor, rooms);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Corridor: ").append(corridor.toString()).append(" ");
        for (Amphipod amphipod : Amphipod.values()) {
            Room room = rooms.get(amphipod);
            sb.append(" ").append(room.toString());
        }
        return sb.toString();
    }

    public String prettyToString() {
        StringBuilder sb = new StringBuilder();
        sb.append("#############\n");

        sb.append("#").append(corridor.toString()).append("#\n");

        sb.append("###");
        for (Amphipod amphipod : Amphipod.values()) {
            sb.append(rooms.get(amphipod).getOuter().toString()).append("#");
        }
        sb.append("##\n");

        sb.append("  #");
        for (Amphipod amphipod : Amphipod.values()) {
            sb.append(rooms.get(amphipod).getInner().toString()).append("#");
        }
        sb.append("  \n");
        sb.append("  #########  ");
        return sb.toString();
    }

    public int getNumberOfCorrect() {
        int numberOfCorrect = 0;
        for (Amphipod roomType : Amphipod.values()) {
            Room room = rooms.get(roomType);
            if (room.getInner() == roomType) {
                numberOfCorrect++;
                if (room.getOuter() == roomType) {
                    numberOfCorrect++;
                }
            }
        }
        return numberOfCorrect;
    }

    public Integer estimateRemaining() {
        int totalCost = 0;
        //Estimate rooms
        for (Amphipod roomType : Amphipod.values()) {
            Room room = rooms.get(roomType);
            int startPosition = room.getPosition();
            int endPosition;
            int distance;
            int cost;
            if (room.getInner() != null) {
                endPosition = rooms.get(room.getInner()).getPosition();
                distance = Math.abs(startPosition - endPosition);
                cost = distance * (int) Math.pow(10, room.getInner().ordinal());
                totalCost += cost;
            }
            if (room.getOuter() != null) {
                endPosition = rooms.get(room.getOuter()).getPosition();
                distance = Math.abs(startPosition - endPosition);
                cost = distance * (int) Math.pow(10, room.getOuter().ordinal());
                totalCost += cost;
            }
        }
        //Estimate corridor
        for (int position = 0; position < corridor.length(); position++) {
            Amphipod amphipod = corridor.get(position);
            if (amphipod == null) {
                continue;
            }
            Room targetRoom = rooms.get(amphipod);
            int endPosition = targetRoom.getPosition();
            int distance = Math.abs(position - endPosition);
            int cost = distance * (int) Math.pow(10, amphipod.ordinal());
            totalCost += cost;
        }
        return totalCost;
    }
}

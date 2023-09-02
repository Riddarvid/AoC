package aoc.days.day4;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day4 extends Day {
    private List<Room> rooms;
    private List<Room> realRooms;

    public static void main(String[] args) {
        new Day4();
    }

    @Override
    protected void part1() {
        List<Room> realRooms = new ArrayList<>();
        for (Room room : rooms) {
            if (room.isValid()) {
                realRooms.add(room);
            }
        }
        this.realRooms = realRooms;
        int sum = 0;
        for (Room room : realRooms) {
            sum += room.getSectorID();
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        for (Room room : realRooms) {
            room.decrypt();
            if (room.getDecryptedName().contains("pole")) {
                System.out.println(room.getSectorID());
                break;
            }
        }
    }

    @Override
    protected void setup() {
        rooms = new ArrayList<>();
        for (String s : lines) {
            rooms.add(new Room(s));
        }
    }
}

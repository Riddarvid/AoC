package days.day5;

import riddarvid.aoc.days.Day;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day5 extends Day {
    List<Seat> seats;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    public long part1() {
        int highest = 0;
        for (Seat seat : seats) {
            int id = seat.getSeatID();
            if (id > highest) {
                highest = id;
            }
        }
        return highest;
    }

    @Override
    public long part2() {
        Set<Integer> ids = new HashSet<>();
        for (Seat s : seats) {
            ids.add(s.getSeatID());
        }
        int seat = 0;
        while (!ids.contains(seat)) {
            seat++;
        }
        while (ids.contains(seat)) {
            seat++;
        }
        return seat;
    }

    @Override
    public void setup() {
        seats = new ArrayList<>();
        for (String s : lines) {
            seats.add(new Seat(s));
        }
    }
}

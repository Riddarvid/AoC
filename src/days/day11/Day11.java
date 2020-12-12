package days.day11;

import riddarvid.aoc.days.Day;

public class Day11 extends Day {
    private Seat[][] seats;

    public static void main(String[] args) {
        new Day11().runAndPrint();
    }

    @Override
    public long part1() {
        boolean hasChanged = true;
        while (hasChanged) {
            hasChanged = step1();
        }
        return getOccupied();
    }

    private void printSeats() {
        for (int row = 0; row < seats.length; row++) {
            for (int col = 0; col < seats[0].length; col++) {
                if (seats[row][col] == null) {
                    System.out.print('.');
                } else if (seats[row][col].isOccupied()) {
                    System.out.print('#');
                } else {
                    System.out.print('L');
                }
            }
            System.out.println();
        }
        System.out.println();
    }

    private long getOccupied() {
        long sum = 0;
        for (int row = 0; row < seats.length; row++) {
            for (int col = 0; col < seats[0].length; col++) {
                if (seats[row][col] != null && seats[row][col].isOccupied()) {
                    sum++;
                }
            }
        }
        return sum;
    }

    private boolean step1() {
        Seat[][] newSeats = new Seat[seats.length][];
        boolean hasChanged = false;
        for (int row = 0; row < seats.length; row++) {
            newSeats[row] = new Seat[seats[0].length];
            for (int col = 0; col < seats[0].length; col++) {
                Seat seat = seats[row][col];
                if (seat == null) {
                    continue;
                }
                int count = getCount1(row, col);
                Seat newSeat;
                if (seat.isOccupied() && count >= 4) {
                    newSeat = new Seat(false);
                    hasChanged = true;
                } else if (!seat.isOccupied() && count == 0) {
                    newSeat = new Seat(true);
                    hasChanged = true;
                } else {
                    newSeat = seat;
                }
                newSeats[row][col] = newSeat;
            }
        }
        seats = newSeats;
        return hasChanged;
    }

    private int getCount1(int row, int col) {
        int sum = 0;
        for (int y = -1; y <= 1; y++) {
            for (int x = -1; x <= 1; x++) {
                if (row + y < 0 || row + y >= seats.length || col + x < 0 || col + x >= seats[0].length || (x == 0 && y == 0)) {
                    continue;
                }
                if (seats[row + y][col + x] != null && seats[row + y][col + x].isOccupied()) {
                    sum++;
                }
            }
        }
        return sum;
    }

    @Override
    public long part2() {
        setup();
        boolean hasChanged = true;
        while (hasChanged) {
            hasChanged = step2();
        }
        return getOccupied();
    }

    private boolean step2() {
        Seat[][] newSeats = new Seat[seats.length][];
        boolean hasChanged = false;
        for (int row = 0; row < seats.length; row++) {
            newSeats[row] = new Seat[seats[0].length];
            for (int col = 0; col < seats[0].length; col++) {
                Seat seat = seats[row][col];
                if (seat == null) {
                    continue;
                }
                int count = getCount2(row, col);
                Seat newSeat;
                if (seat.isOccupied() && count >= 5) {
                    newSeat = new Seat(false);
                    hasChanged = true;
                } else if (!seat.isOccupied() && count == 0) {
                    newSeat = new Seat(true);
                    hasChanged = true;
                } else {
                    newSeat = seat;
                }
                newSeats[row][col] = newSeat;
            }
        }
        seats = newSeats;
        return hasChanged;
    }

    private int getCount2(int row, int col) {
        int count = 0;
        for (int y = -1; y <= 1; y++) {
            for (int x = -1; x <= 1; x++) {
                if (x == 0 && y == 0) {
                    continue;
                }
                count += countDirection(row, col, y, x);
            }
        }
        return count;
    }

    private int countDirection(int row, int col, int dy, int dx) {
        row += dy;
        col += dx;
        while (row >= 0 && row < seats.length && col >= 0 && col < seats[0].length) {
            if (seats[row][col] != null) {
                if (seats[row][col].isOccupied()) {
                    return 1;
                }
                return 0;
            }
            row += dy;
            col += dx;
        }
        return 0;
    }

    @Override
    public void setup() {
        seats = new Seat[lines.size()][];
        for (int rowIndex = 0; rowIndex < lines.size(); rowIndex++) {
            String s = lines.get(rowIndex);
            Seat[] row = new Seat[s.length()];
            for (int colIndex = 0; colIndex < row.length; colIndex++) {
                if (s.charAt(colIndex) == 'L') {
                    row[colIndex] = new Seat(false);
                } else if (s.charAt(colIndex) == '#') {
                    row[colIndex] = new Seat(true);
                }
            }
            seats[rowIndex] = row;
        }
    }
}

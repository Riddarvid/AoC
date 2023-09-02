package aoc.days.day16;

import aoc.days.Day;

public class Day16 extends Day {
    private String startingData;
    private int diskLength1;
    private int diskLength2;

    public static void main(String[] args) {
        new Day16();
    }

    @Override
    protected void part1() {
        String data = dragonCurve(startingData, diskLength1);
        data = data.substring(0, diskLength1);
        System.out.println(checksum(data));
    }

    private String checksum(String data) {
        if (data.length() % 2 == 1) {
            return data;
        }
        StringBuilder checksum = new StringBuilder();
        for (int i = 0; i < data.length(); i += 2) {
            if (data.charAt(i) == data.charAt(i + 1)) {
                checksum.append('1');
            } else {
                checksum.append('0');
            }
        }
        return checksum(checksum.toString());
    }

    private String dragonCurve(String startingData, int diskLength) {
        String a = startingData;
        while (a.length() < diskLength) {
            String b = getOpposite(a);
            a = a + '0' + b;
        }
        return a;
    }

    private String getOpposite(String a) {
        StringBuilder b = new StringBuilder();
        for (int i = a.length() - 1; i >= 0; i--) {
            if (a.charAt(i) == '0') {
                b.append('1');
            } else {
                b.append('0');
            }
        }
        return b.toString();
    }

    @Override
    protected void part2() {
        String data = dragonCurve(startingData, diskLength2);
        data = data.substring(0, diskLength2);
        System.out.println(checksum(data));
    }

    @Override
    protected void setup() {
        startingData = lines.get(0);
        diskLength1 = 272;
        diskLength2 = 35651584;
    }
}

package aoc.days.day7;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;

public class Day7 extends Day {
    private List<Address> addresses;

    public static void main(String[] args) {
        new Day7();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (Address address : addresses) {
            if (address.supportsTLS()) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        int sum = 0;
        for (Address address : addresses) {
            if (address.supportsSSL()) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void setup() {
        addresses = new ArrayList<>();
        for (String s : lines) {
            addresses.add(new Address(s));
        }
    }
}

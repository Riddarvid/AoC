package aoc.days.day6;

import aoc.days.Day;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Day6 extends Day {
    private List<String> stringsPosition;

    public static void main(String[] args) {
        new Day6();
    }

    @Override
    protected void part1() {
        StringBuilder sb = new StringBuilder();
        for (String s : stringsPosition) {
            Queue<Character> pq = new PriorityQueue<>(new StringComparator(s, false));
            for (char c = 'a'; c <= 'z'; c++) {
                pq.add(c);
            }
            sb.append(pq.poll());
        }
        System.out.println(sb.toString());
    }

    @Override
    protected void part2() {
        StringBuilder sb = new StringBuilder();
        for (String s : stringsPosition) {
            Queue<Character> pq = new PriorityQueue<>(new StringComparator(s, true));
            for (char c = 'a'; c <= 'z'; c++) {
                pq.add(c);
            }
            sb.append(pq.poll());
        }
        System.out.println(sb.toString());
    }

    @Override
    protected void setup() {
        stringsPosition = new ArrayList<>();
        for (int column = 0; column < lines.get(0).length(); column++) {
            StringBuilder sb = new StringBuilder();
            for (String s : lines) {
                sb.append(s.charAt(column));
            }
            stringsPosition.add(sb.toString());
        }
    }
}

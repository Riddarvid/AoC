package aoc.days.Day20;

import aoc.FileUtilities;

import java.io.File;
import java.util.*;

public class Day20 {
    List<Point> discovered;
    List<Point> toBeDrawn = new ArrayList<>();
    boolean redrawGraph = true;
    boolean done = false;
    BaseView view;
    Point longest;

    public static void main(String[] args) {
        new Day20();
    }

    public Day20() {
        char[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day20/input.txt"))[0].toCharArray();
        int i = 0;
        Deque<Character> stack = new ArrayDeque<>();
        discovered = new ArrayList<>();
        Point current = new Point(150, 60);
        Point start = current;
        discovered.add(current);
        view = new BaseView(this);
        while (i < input.length) {
            /*try {
                Thread.sleep(1);
            } catch (InterruptedException e) {

            }*/
            char c = input[i];
            switch (c) {
                case '(':
                    stack.addFirst(c);
                    break;
                case '|':
                    current = backtrack(stack, current);
                    break;
                case ')':
                    current = backtrack(stack, current);
                    stack.removeFirst();
                    break;
                case 'N':
                case 'E':
                case 'S':
                case 'W':
                    stack.addFirst(c);
                    current = explore(c, discovered, current);
                    break;
            }
            view.update();
            i++;
        }
        redrawGraph = true;
        //view.update();
        dijkstra(discovered, start);
        Queue<Point> pq = new PriorityQueue<>(new CompDijkstraLong());
        pq.addAll(discovered);
        System.out.println("Longest distance: " + pq.peek().distance);
        int count = 0;
        longest = pq.peek();
        /*while (pq.peek().distance >= 1000) {
            count++;
            pq.poll();
        }
        System.out.println("Number of rooms that require you to pass through at least 1000 doors: " + count);*/
        done = true;
        view.update();
    }

    private void dijkstra(List<Point> graph, Point start) {
        List<Point> unknown = new ArrayList<>();
        for (Point p : graph) {
            unknown.add(p);
        }
        toBeDrawn.add(start);
        unknown.remove(start);
        start.distance = 0;
        for (Point p : unknown) { //init
            p.previous = start;
            p.distance = cost(start, p);
        }
        Queue<Point> pq;
        while (!unknown.isEmpty()) {
            /*try {
                Thread.sleep(1);
            } catch (InterruptedException e) {

            }*/
            pq = new PriorityQueue<>(new CompDijkstraShort());
            pq.addAll(unknown);
            Point w = pq.poll();
            toBeDrawn.add(w);
            view.update();
            unknown.remove(w);
            for (Point v : unknown) {
                if (w.distance + cost(w, v) < v.distance) {
                    v.distance = w.distance + cost(w, v);
                    v.previous = w;
                }
            }
        }
    }

    private int cost(Point a, Point b) {
        if (a.north != null && a.north.equals(b)) {
            return 1;
        }
        if (a.east != null && a.east.equals(b)) {
            return 1;
        }
        if (a.south != null && a.south.equals(b)) {
            return 1;
        }
        if (a.west != null && a.west.equals(b)) {
            return 1;
        }
        return 10000000;
    }

    private Point backtrack(Deque<Character> stack, Point current) {
        Point goal = current;
        Character c;
        while (stack.peekFirst() != '(') {
            if (goal == null) {
                System.out.println("Blä");
            }
            c = stack.pollFirst();
            switch (c) {
                case 'N':
                    goal = goal.south;
                    break;
                case 'E':
                    goal = goal.west;
                    break;
                case 'S':
                    goal = goal.north;
                    break;
                case 'W':
                    goal = goal.east;
                    break;
            }
        }
        return goal;
    }

    private Point explore(char c, List<Point> discovered, Point current) {
        Point p;
        switch (c) {
            case 'N':
                p = new Point(current.x, current.y - 1);
                if (discovered.contains(p)) {
                    p = discovered.get(discovered.indexOf(p));
                } else {
                    discovered.add(p);
                }
                current.north = p;
                p.south = current;
                return p;
            case 'E':
                p = new Point(current.x + 1, current.y);
                if (discovered.contains(p)) {
                    p = discovered.get(discovered.indexOf(p));
                } else {
                    discovered.add(p);
                }
                current.east = p;
                p.west = current;
                return p;
            case 'S':
                p = new Point(current.x, current.y + 1);
                if (discovered.contains(p)) {
                    p = discovered.get(discovered.indexOf(p));
                } else {
                    discovered.add(p);
                }
                current.south = p;
                p.north = current;
                return p;
            case 'W':
                p = new Point(current.x - 1, current.y);
                if (discovered.contains(p)) {
                    p = discovered.get(discovered.indexOf(p));
                } else {
                    discovered.add(p);
                }
                current.west = p;
                p.east = current;
                return p;
        }
        return null;
    }
}

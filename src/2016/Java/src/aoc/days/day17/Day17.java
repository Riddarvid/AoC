package aoc.days.day17;

import aoc.days.Day;
import aoc.utils.geometry.Point2D;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.util.*;

public class Day17 extends Day {
    private String salt;
    List<Character> keys;

    public static void main(String[] args) {
        new Day17();
    }

    @Override
    protected void part1() {
        System.out.println(shortest(new Point2D(3, 3), salt));
    }

    private String shortest(Point2D goal, String salt) {
        Set<String> toVisit = new HashSet<>();
        toVisit.add("");
        while (!toVisit.isEmpty()) {
            Set<String> toVisitNext = new HashSet<>();
            for (String path : toVisit) {
                if (isAtGoal(path, goal)) {
                    return path;
                }
                List<String> neighbours = getNeighbours(path, salt, goal);
                toVisitNext.addAll(neighbours);
            }
            toVisit = toVisitNext;
        }
        throw new InputMismatchException("No path exists");
    }

    private String longest(Point2D goal, String salt) {
        Set<String> toVisit = new HashSet<>();
        toVisit.add("");
        String longest = "";
        while (!toVisit.isEmpty()) {
            Set<String> toVisitNext = new HashSet<>();
            for (String path : toVisit) {
                if (isAtGoal(path, goal)) {
                    longest = path;
                    continue;
                }
                List<String> neighbours = getNeighbours(path, salt, goal);
                toVisitNext.addAll(neighbours);
            }
            toVisit = toVisitNext;
        }
        return longest;
    }

    private List<String> getNeighbours(String path, String salt, Point2D goal) {
        String hash = bytesToHex(hash(salt + path));
        Point2D position = getPosition(path);
        List<String> neighbours = new ArrayList<>();
        if (position.getY() != 0 && keys.contains(hash.charAt(0))) {
            neighbours.add(path + "U");
        }
        if (position.getY() != goal.getY() && keys.contains(hash.charAt(1))) {
            neighbours.add(path + "D");
        }
        if (position.getX() != 0 && keys.contains(hash.charAt(2))) {
            neighbours.add(path + "L");
        }
        if (position.getX() != goal.getX() && keys.contains(hash.charAt(3))) {
            neighbours.add(path + "R");
        }
        return neighbours;
    }

    private Point2D getPosition(String path) {
        int x = 0;
        int y = 0;
        for (char c : path.toCharArray()) {
            switch (c) {
                case 'U':
                    y--;
                    break;
                case 'D':
                    y++;
                    break;
                case 'L':
                    x--;
                    break;
                case 'R':
                    x++;
                    break;
                default:
                    throw new InputMismatchException("" + c);
            }
        }
        return new Point2D(x, y);
    }

    private boolean isAtGoal(String path, Point2D goal) {
        return getPosition(path).equals(goal);
    }

    private byte[] hash(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            return md.digest(input.getBytes());
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
        throw new InputMismatchException();
    }

    private String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    @Override
    protected void part2() {
        System.out.println(longest(new Point2D(3, 3), salt).length());
    }

    @Override
    protected void setup() {
        salt = lines.get(0);
        keys = new ArrayList<>();
        for (char c = 'b'; c <= 'f'; c++) {
            keys.add(c);
        }
    }
}

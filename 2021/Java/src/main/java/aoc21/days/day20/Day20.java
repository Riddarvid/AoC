package aoc21.days.day20;

import aoc.days.Day;
import aoc.math.geometry.Point;

import java.util.HashSet;
import java.util.Set;

public class Day20 extends Day {
    private String enhancementAlgorithm;
    private Set<Point> image;

    private boolean darkMode;

    private void printImage(Set<Point> image) {
        int minX = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (Point point : image) {
            int x = point.getX();
            int y = point.getY();
            if (x < minX) {
                minX = x;
            }
            if (x > maxX) {
                maxX = x;
            }
            if (y < minY) {
                minY = y;
            }
            if (y > maxY) {
                maxY = y;
            }
        }
        System.out.println();
        int border = 3;
        for (int y = minY - border; y <= maxY + border; y++) {
            for (int x = minX - border; x <= maxX + border; x++) {
                if (image.contains(new Point(x, y))) {
                    if (darkMode) {
                        System.out.print('.');
                    } else {
                        System.out.print('#');
                    }
                } else {
                    if (darkMode) {
                        System.out.print('#');
                    } else {
                        System.out.print('.');
                    }
                }
            }
            System.out.println();
        }
        System.out.println();
    }

    @Override
    public void setup() {
        enhancementAlgorithm = lines.get(0);
        image = new HashSet<>();
        for (int y = 2; y < lines.size(); y++) {
            for (int x = 0; x < lines.get(y).length(); x++) {
                if (lines.get(y).charAt(x) == '#') {
                    image.add(new Point(x, y));
                }
            }
        }
        darkMode = false;
    }

    private Set<Point> enhance(Set<Point> image, int times) {
        for (int i = 0; i < times; i++) {
            image = enhance(image);
        }
        return image;
    }

    private Set<Point> enhance(Set<Point> image) {
        Set<Point> toConsider = new HashSet<>();
        for (Point point : image) {
            toConsider.addAll(getNeighbours(point));
        }
        Set<Point> enhanced = new HashSet<>();
        for (Point point : toConsider) {
            int enhancementIndex = getEnhancementIndex(point, image);
            if (darkMode) {
                if (enhancementAlgorithm.charAt(enhancementIndex) == '#') {
                    enhanced.add(point);
                }
            } else {
                if (enhancementAlgorithm.charAt(enhancementIndex) == '.') {
                    enhanced.add(point);
                }
            }
        }
        darkMode = !darkMode;
        return enhanced;
    }

    private Set<Point> getNeighbours(Point point) {
        Set<Point> neighbours = new HashSet<>();
        for (int x = point.getX() - 2; x <= point.getX() + 2; x++) {
            for (int y = point.getY() - 2; y <= point.getY() + 2; y++) {
                neighbours.add(new Point(x, y));
            }
        }
        return neighbours;
    }

    private int getEnhancementIndex(Point point, Set<Point> image) {
        StringBuilder binaryString = new StringBuilder();
        for (int y = point.getY() - 1; y <= point.getY() + 1; y++) {
            for (int x = point.getX() - 1; x <= point.getX() + 1; x++) {
                if (darkMode) {
                    if (image.contains(new Point(x, y))) {
                        binaryString.append(0);
                    } else {
                        binaryString.append(1);
                    }
                } else {
                    if (image.contains(new Point(x, y))) {
                        binaryString.append(1);
                    } else {
                        binaryString.append(0);
                    }
                }
            }
        }
        return Integer.parseInt(binaryString.toString(), 2);
    }

    @Override
    public long part1() {
        Set<Point> image = enhance(this.image, 2);
        return image.size();
    }

    @Override
    public long part2() {
        Set<Point> image = enhance(this.image, 50);
        printImage(image);
        return image.size();
    }
}

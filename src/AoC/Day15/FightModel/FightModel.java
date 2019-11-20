package AoC.Day15.FightModel;

import AoC.FileUtilities;
import AoC.RegEx;

import java.awt.*;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;

public class FightModel {
    private List<Point> map = new ArrayList<>();
    private List<Unit> units = new ArrayList<>();
    private int atkPwr;
    private int numberOfElves;

    public FightModel(int atkPwr) {
        char[][] input = RegEx.getCharArr(FileUtilities.getArrayFromFile(new File("src/AoC/Day15/Input.txt")));
        initModel(input, atkPwr);
    }

    private void initModel(char[][] input, int atkPwr) {
        numberOfElves = 0;
        for (int y = 0; y < input.length; y++) {
            for (int x = 0; x < input[y].length; x++) {
                char c = input[y][x];
                if (c != '#') {
                    Point p = new Point(x, y);
                    map.add(p);
                    if (c == 'E') {
                        units.add(new Unit(p, Color.RED, atkPwr));
                        numberOfElves++;
                    } else if (c == 'G') {
                        units.add(new Unit(p, Color.GREEN, 3));
                    }
                }
            }
        }
        Unit.setUnits(units);
        createConnections();
    }

    private void createConnections() {
        for (Point p : map) {
            for (Point o : map) {
                if (p.getX() == o.getX() && p.getY() == (o.getY() - 1)) {
                    p.south = o;
                }
                if (p.getX() == o.getX() && p.getY() == (o.getY() + 1)) {
                    p.north = o;
                }
                if (p.getX() == (o.getX() - 1) && p.getY() == (o.getY())) {
                    p.east = o;
                }
                if (p.getX() == (o.getX() + 1) && p.getY() == (o.getY())) {
                    p.west = o;
                }
            }
        }
    }

    public void update() {
        PriorityQueue<Unit> unitQueue = new PriorityQueue<>(new ReadComparator());
        unitQueue.addAll(units);
        while (!unitQueue.isEmpty()) {
            Unit u = unitQueue.poll();
            if (!u.isDead()) {
                if (u.isInRange()) {
                    u.attack();
                } else {
                    u.move();
                    if (u.isInRange()) {
                        u.attack();
                    }
                }
            }
        }
        List<Unit> toBeRemoved = new ArrayList<>();
        for (Unit u : units) {
            if (u.isDead()) {
                toBeRemoved.add(u);
            }
        }
        for (Unit u : toBeRemoved) {
            units.remove(u);
        }
    }

    public List<Point> getMap() {
        return map;
    }

    public List<Unit> getUnits() {
        return units;
    }

    public int getTotalHP() {
        int sum = 0;
        for (Unit u : units) {
            sum += u.getHealth();
        }
        return sum;
    }

    public int getNumberOfElves() {
        return numberOfElves;
    }
}

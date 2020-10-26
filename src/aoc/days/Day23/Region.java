package aoc.days.Day23;

import java.util.ArrayList;
import java.util.Collection;

import static aoc.days.Day23.Day23.isInRange;

public class Region implements Comparable<Region> {
    private Point point;
    private int size;
    private int numberOfBotsInRange;

    public Region(Point point, int size, Collection<NanoBot> nanoBots) {
        this.point = point;
        this.size = size;
        calculateNumberOfBotsInRange(nanoBots);
    }

    private void calculateNumberOfBotsInRange(Collection<NanoBot> nanoBots) {
        int inRange = 0;
        for (NanoBot bot : nanoBots) {
            Point p = getClosestPoint(bot);
            if (isInRange(bot, p)) {
                inRange++;
            }
        }
        numberOfBotsInRange = inRange;
    }

    private Point getClosestPoint(NanoBot bot) {
        int x, y, z;
        Point botPoint = bot.getPoint();
        x = getClosest(botPoint.getX());
        y = getClosest(botPoint.getY());
        z = getClosest(botPoint.getZ());
        return new Point(x, y, z);
    }

    private int getClosest(int botCoordinate) { //Felaktig, använder bara x
        int coordinate;
        if (point.getX() > botCoordinate) {
            coordinate = point.getX();
        } else if (point.getX() + size - 1 < botCoordinate) {
            coordinate = point.getX() + size;
        } else {
            coordinate = botCoordinate;
        }
        return coordinate;
    }

    @Override
    public int compareTo(Region other) {
        return other.numberOfBotsInRange - numberOfBotsInRange;
    }

    public Point getPoint() {
        return point;
    }

    public void setPoint(Point point) {
        this.point = point;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getNumberOfBotsInRange() {
        return numberOfBotsInRange;
    }

    public void setNumberOfBotsInRange(int numberOfBotsInRange) {
        this.numberOfBotsInRange = numberOfBotsInRange;
    }

    @Override
    public String toString() {
        return point.toString() + " Size: " + size + " Number of bots in range: " + numberOfBotsInRange;
    }

    public Collection<Region> getSubRegions(Collection<NanoBot> nanoBots) {
        Collection<Region> subRegions = new ArrayList<>();
        int newSize = (int)Math.ceil((double)size / 2.0);
        for (int x = point.getX(); x < point.getX() + size; x += newSize) {
            for (int y = point.getY(); y < point.getY() + size; y += newSize) {
                for (int z = point.getZ(); z < point.getZ() + size; z += newSize) {
                    subRegions.add(new Region(new Point(x, y, z), newSize, nanoBots));
                }
            }
        }
        return subRegions;
    }
}

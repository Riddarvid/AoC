package AoC.Day23alt;

import java.util.ArrayList;
import java.util.List;

public class Cube implements Comparable<Cube> {
    private int minX, maxX, minY, maxY, minZ, maxZ, botsInRange;
    public static int numberOfCubes = 0;

    public Cube(int minX, int maxX, int minY, int maxY, int minZ, int maxZ, List<NanoBot> nanoBots) {
        numberOfCubes++;
        this.minX = minX;
        this.maxX = maxX;
        this.minY = minY;
        this.maxY = maxY;
        this.minZ = minZ;
        this.maxZ = maxZ;
        calculateBotsInRange(nanoBots);
    }

    private void calculateBotsInRange(List<NanoBot> nanoBots) {
        int botsInRange = 0;
        for (NanoBot bot : nanoBots) {
            Point p = getClosestPoint(bot);
            if (p.distance(bot.getPoint()) <= bot.getRadius()) {
                botsInRange++;
            }
        }
        this.botsInRange = botsInRange;
    }

    private Point getClosestPoint(NanoBot bot) {
        int x, y, z;
        if (bot.getX() < minX) {
            x = minX;
        } else if (bot.getX() > maxX) {
            x =maxX;
        } else {
            x = bot.getX();
        }
        if (bot.getY() < minY) {
            y = minY;
        } else if (bot.getY() > maxY) {
            y =maxY;
        } else {
            y = bot.getY();
        }
        if (bot.getZ() < minZ) {
            z = minZ;
        } else if (bot.getZ() > maxZ) {
            z =maxZ;
        } else {
            z = bot.getZ();
        }
        return new Point(x, y, z);
    }

    public int getBotsInRange() {
        return botsInRange;
    }

    public int getSide() {
        return maxX - minX + 1;
    }

    public List<Cube> getSubCubes(List<NanoBot> nanoBots) {
        List<Cube> subCubes = new ArrayList<>();
        if (getSide() == 1) {
            return subCubes;
        }
        int side = getSide() / 2;
        for (int x = minX; x <= maxX; x += side) {
            for (int y = minY; y <= maxY; y += side) {
                for (int z = minZ; z <= maxZ; z += side) {
                    subCubes.add(new Cube(x, x + side - 1, y, y + side - 1, z, z + side - 1, nanoBots));
                }
            }
        }
        return subCubes;
    }

    @Override
    public int compareTo(Cube o) {
        return o.botsInRange - botsInRange;
    }

    @Override
    public String toString() {
        return "Side: " + getSide() + "\tBots in range: " + botsInRange;
    }

    public int getDistanceFromOrigo() {
        Point origo = new Point(0, 0, 0);
        Point p = new Point(maxX, maxY, maxZ);
        return p.distance(origo);
    }
}

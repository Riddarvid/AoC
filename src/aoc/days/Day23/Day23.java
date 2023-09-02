package aoc.days.Day23;

import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

public class Day23 {
    public static void main(String[] args) {
        new Day23();
    }

    List<NanoBot> nanoBots = new ArrayList<>();
    private int scale = 1;

    public Day23() {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day23/input.txt"));
        /*try {
            FileWriter fw = new FileWriter("src/AoC/Day23/Output.txt");
            for (String s : input) {
                List<Integer> values = RegEx.getIntegers(s);
                fw.write(values.get(0) + "," + values.get(1) + "," + values.get(2) + "," + values.get(3) + "\n");
            }
            fw.close();
        } catch (IOException e) {

        }
        System.exit(0);*/
        for (String s : input) {
            List<Integer> values = RegEx.getIntegers(s);
            nanoBots.add(new NanoBot(new Point(values.get(0) / scale, values.get(1) / scale, values.get(2) / scale), values.get(3) / scale));
        }
        try {
            FileWriter fw = new FileWriter("src/AoC/Day23/Test.txt");
            NanoBot bot = nanoBots.get(0);
            for (int x = bot.getPoint().getX() - bot.getRadius(); x < bot.getPoint().getX() + bot.getRadius(); x++) {
                for (int y = bot.getPoint().getY() - bot.getRadius() + x; y < bot.getPoint().getY() + bot.getRadius() - x; y++) {
                    for (int z = bot.getPoint().getZ() - bot.getRadius() + x + y; z < bot.getPoint().getZ() + bot.getRadius() - x - y; z++) {
                        fw.write(x + "," + y + "," + z + "\n");
                    }
                }
            }
            /*for (NanoBot bot : nanoBots) {
                fw.write(values.get(0) + "," + values.get(1) + "," + values.get(2) + "," + values.get(3) + "\n");
            }*/
            fw.close();
        } catch (IOException e) {

        }
        System.exit(0);
        /*Queue<NanoBot> pq = new PriorityQueue<>(new CompBotRange());
        pq.addAll(nanoBots);
        System.out.println(pq.peek());
        System.out.println(getBotsInRange(pq.peek()).size());*/
        Point origo = new Point(0, 0, 0);
        int minX, minY, minZ, maxX, maxY, maxZ, minRadius, maxRadius, minFromZero, maxFromZero;
        minX = maxX = nanoBots.get(0).getPoint().getX();
        minY = maxY = nanoBots.get(0).getPoint().getY();
        minZ = maxZ = nanoBots.get(0).getPoint().getZ();
        minRadius = maxRadius = nanoBots.get(0).getRadius();
        minFromZero = maxFromZero = distance(origo, nanoBots.get(0).getPoint());
        for (NanoBot bot : nanoBots) {
            if (bot.getPoint().getX() < minX) {
                minX = bot.getPoint().getX();
            }
            if (bot.getPoint().getY() < minY) {
                minY = bot.getPoint().getY();
            }
            if (bot.getPoint().getZ() < minZ) {
                minZ = bot.getPoint().getZ();
            }
            if (bot.getPoint().getX() > maxX) {
                maxX = bot.getPoint().getX();
            }
            if (bot.getPoint().getY() > maxY) {
                maxY = bot.getPoint().getY();
            }
            if (bot.getPoint().getZ() > maxZ) {
                maxZ = bot.getPoint().getZ();
            }
            if (bot.getRadius() > maxRadius) {
                maxRadius = bot.getRadius();
            }
            if (bot.getRadius() < minRadius) {
                minRadius = bot.getRadius();
            }
            if (distance(origo, bot.getPoint()) < minFromZero) {
                minFromZero = distance(origo, bot.getPoint());
            }
            if (distance(origo, bot.getPoint()) > maxFromZero) {
                maxFromZero = distance(origo, bot.getPoint());
            }
        }
        System.out.println("Yo");
        int numberOfRegionsX = 2;
        int sizeOfRegions = (int)Math.ceil((double)(maxX - minX) / numberOfRegionsX);
        int numberOfRegionsY = (int)Math.ceil((double)(maxY - minY) / sizeOfRegions);
        int numberOfRegionsZ = (int)Math.ceil((double)(maxZ - minZ) / sizeOfRegions);
        List<Region> regions = new ArrayList<>();
        for (int x = 0; x < numberOfRegionsX; x++) {
            for (int y = 0; y < numberOfRegionsY; y++) {
                for (int z = 0; z < numberOfRegionsZ; z++) {
                    regions.add(new Region(new Point(x * sizeOfRegions + minX, y * sizeOfRegions + minY, z * sizeOfRegions + minZ), sizeOfRegions, nanoBots));
                }
            }
        }
        System.out.println("YoYo");
        getBestRegions(regions);
        /*List<Point> points = new ArrayList<>();
        int i = 1;
        for (NanoBot bot : nanoBots) {
            System.out.println(((double)i)/1000.0 * 100.0);
            i++;
            bot.updateRange(points);
        }
        System.out.println("wow");*/
    }

    private Collection<Region> getBestRegions(List<Region> regions) {
        while (!isDone(regions, 1000000)) {
            System.out.println("Number of points remaining: " + getNumberOfPoints(regions));
            printDisparate(regions);
            Queue<Region> pq = new PriorityQueue<>(regions); //TODO ersätt med att gå igenom listan och hitta största tal
            if (pq.isEmpty()) {
                System.out.println("hm");
            }
            regions.clear();
            int maxInRange = pq.peek().getNumberOfBotsInRange();
            while (pq.peek().getNumberOfBotsInRange() == maxInRange) {
                regions.add(pq.poll());
            }
            Collection<Region> subRegions = new ArrayList<>();
            for (Region region : regions) {
                subRegions.addAll(region.getSubRegions(nanoBots));
            }
            regions.clear();
            regions.addAll(subRegions);
        }
        return regions;
    }

    private void printDisparate(List<Region> regions) {
        List<Integer> disparate = new ArrayList<>();
        disparate.add(regions.get(0).getNumberOfBotsInRange());
        System.out.println(regions.get(0).getNumberOfBotsInRange());
        for (Region r : regions) {
            if (!disparate.contains(r.getNumberOfBotsInRange())) {
                disparate.add(r.getNumberOfBotsInRange());
                System.out.println(r.getNumberOfBotsInRange());
            }
        }
    }

    private BigInteger getNumberOfPoints(List<Region> regions) {
        BigInteger size = BigInteger.valueOf(regions.get(0).getSize());
        size = size.multiply(size);
        size = size.multiply(size);
        size = size.multiply(BigInteger.valueOf(regions.size()));
        return size;
    }

    private boolean isDone(List<Region> regions, int threshhold) {
        if (regions.get(0).getSize() > threshhold) {
            return false;
        }
        int numberOfPoints = 0;
        for (Region region : regions) {
            int size = region.getSize();
            numberOfPoints += size * size * size;
            if (numberOfPoints > threshhold) {
                return false;
            }
        }
        return true;
    }


    private Collection<NanoBot> getBotsInRange(NanoBot bot) {
        Collection<NanoBot> inRange = new ArrayList<>();
        for (NanoBot n : nanoBots) {
            if (isInRange(bot, n)) {
                inRange.add(n);
            }
        }
        return inRange;
    }

    int getNumberOfBotsInRange(Point p) {
        int numberInRange = 0;
        for (NanoBot bot : nanoBots) {
            if (isInRange(bot, p)) {
                numberInRange++;
            }
        }
        return numberInRange;
    }

    static boolean isInRange(NanoBot n, Point p) {
        return distance(n, p) <= n.getRadius();
    }

    private static boolean isInRange(NanoBot a, NanoBot b) {
        return distance(a, b) <= a.getRadius();
    }

    private static int distance(NanoBot a, NanoBot b) {
        return distance(a.getPoint(), b.getPoint());
    }

    private static int distance(NanoBot n, Point p) {
        return distance(n.getPoint(), p);
    }

    private static int distance(Point a, Point b) {
        int x = Math.abs(a.getX() - b.getX());
        int y = Math.abs(a.getY() - b.getY());
        int z = Math.abs(a.getZ() - b.getZ());
        return x + y + z;
    }
}

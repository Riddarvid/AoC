package aoc.days.Day23alt;

import aoc.FileUtilities;
import aoc.RegEx;

import java.io.File;
import java.util.*;

public class Day23 {
    static List<NanoBot> nanoBots = new ArrayList<>();

    public static void main(String[] args) {
        new Day23().run();
    }

    private void run() {
        initBots();
        Cube startCube;
        List<Cube> cubes = new ArrayList<>();
        List<Cube> newCubes = new ArrayList<>();
        for (int maxBotsInRange = 1000; maxBotsInRange > 0; maxBotsInRange--) {
            startCube = new Cube(-268435456, 268435455, -268435456, 268435455,-268435456, 268435455, nanoBots);
            cubes.add(startCube);
            while (!cubes.isEmpty()) {
                newCubes = new ArrayList<>();
                for (Cube cube : cubes) {
                    if (cube.getBotsInRange() >= maxBotsInRange) {
                        newCubes.add(cube);
                    }
                }
                if (newCubes.isEmpty()) {
                    maxBotsInRange = getMax(cubes) + 1;
                    cubes = new ArrayList<>();
                    break;
                }
                List<Cube> subCubes = new ArrayList<>();
                for (Cube cube : newCubes) {
                    subCubes.addAll(cube.getSubCubes(nanoBots));
                }
                cubes = subCubes;
            }
            if (!newCubes.isEmpty()) {
                break;
            }
        }
        Cube cube = newCubes.get(0);
        System.out.println(cube.getDistanceFromOrigo());
        System.out.println(Cube.numberOfCubes);
    }

    private int getMax(List<Cube> cubes) {
        int max = cubes.get(0).getBotsInRange();
        for (Cube cube : cubes) {
            if (cube.getBotsInRange() > max) {
                max = cube.getBotsInRange();
            }
        }
        return max;
    }

    private void initBots() {
        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day23/input.txt"));
        for (String s : input) {
            List<Integer> values = RegEx.getIntegers(s);
            NanoBot nanoBot = new NanoBot(values.get(0), values.get(1), values.get(2), values.get(3));
            nanoBots.add(nanoBot);
        }
    }
}

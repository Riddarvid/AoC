package aoc.days.day18;

import riddarvid.aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.io.File;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class Day18 extends Day { //TODO Reducera grafen så att den endast behöver nyckeltecken. Detta innebär att varje ny upptäckt nod kommer innehålla en ny nyckel
                                 //TODO Kanske även hitta direkta avstånd mellan alla par av noder
    private Robot robotA;
    private BRobot robotB;
    private long startTime;

    public static void main(String[] args) {
        new Day18();
    }

    @Override
    protected void part1() {
        System.out.println(robotA.dijkstra());
    }

    @Override
    protected void part2() {
        System.out.println(robotB.dijkstra());
        System.out.println("Time taken: " + (System.nanoTime() - startTime) / 1000000000 + " s");
    }

    @Override
    protected void setup() {
        startTime = System.nanoTime();
        robotA = new Robot(40, 40, lines);
        loadMap2();
        Set<Node> startNodes = new HashSet<>();
        startNodes.add(new Node(39, 39, '@'));
        startNodes.add(new Node(39, 41, '@'));
        startNodes.add(new Node(41, 39, '@'));
        startNodes.add(new Node(41, 41, '@'));
        BDijkstraNode start = new BDijkstraNode(startNodes, new HashSet<>());
        robotB = new BRobot(start, lines);
    }

    private void loadMap2() {
        URL url = getClass().getResource("input2");
        File f = new File(url.getPath());
        lines = InputUtils.getLines(f);
    }
}

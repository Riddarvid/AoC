package aoc.days.day06;

import aoc.days.Day;
import aoc.utils.input.InputUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day6 extends Day {
    private Map<String, Planet> planetMap;

    public static void main(String[] args) {
        new Day6();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (Planet planet : planetMap.values()) {
            sum += planet.getOrbits();
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        List<Planet> pathFromYou = planetMap.get("YOU").getPathToCom();
        List<Planet> pathFromSan = planetMap.get("SAN").getPathToCom();
        int i = 0;
        while (pathFromSan.get(i).equals(pathFromYou.get(i))) {
            i++;
        }
        System.out.println(pathFromYou.size() + pathFromSan.size() - 2 * i - 2);
    }

    @Override
    protected void setup() {
        planetMap = new HashMap<>();
        for (String s : lines) {
            List<String> tokens = InputUtils.getTokens(s, ')');
            Planet a = new Planet(tokens.get(0));
            Planet b = new Planet(tokens.get(1));
            if (!planetMap.containsValue(a)) {
                planetMap.put(a.getId(), a);
            }
            if (!planetMap.containsValue(b)) {
                planetMap.put(b.getId(), b);
            }
        }
        for (String s : lines) {
            List<String> tokens = InputUtils.getTokens(s, ')');
            Planet a = planetMap.get(tokens.get(0));
            Planet b = planetMap.get(tokens.get(1));
            b.setOrbits(a);
        }
    }
}

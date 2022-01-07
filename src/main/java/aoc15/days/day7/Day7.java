package aoc15.days.day7;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;
import aoc15.days.day7.wires.*;

import java.util.List;
import java.util.Map;

public class Day7 extends Day {

    int resultA;

    Map<String, Wire> map;

    @Override
    public long part1() {
        Wire a = Wire.getWire("a");
        resultA = a.getValue();
        return a.getValue();
    }

    @Override
    public long part2() {
        map = Wire.getMap();
        setup();
        new Set("b", Integer.toString(resultA));
        Wire a = map.get("a");
        return a.getValue();
    }

    @Override
    public void setup() {
        for (String instruction : lines) {
            List<String> tokens = ParsingUtils.getTokens(instruction, ' ');
            if (tokens.size() == 3) {
                new Set(tokens.get(2), tokens.get(0));
            } else if (tokens.size() == 4) {
                new Not(tokens.get(3), tokens.get(1));
            } else {
                String id = tokens.get(4);
                String o1 = tokens.get(0);
                String o2 = tokens.get(2);
                switch (tokens.get(1)) {
                    case "AND" -> new And(id, o1, o2);
                    case "OR" -> new Or(id, o1, o2);
                    case "LSHIFT" -> new Lshift(id, o1, o2);
                    case "RSHIFT" -> new Rshift(id, o1, o2);
                    default -> System.out.println("Unsupported operation");
                }
            }
        }
        map = Wire.getMap();
    }
}

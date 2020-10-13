package AoC.day7;

import AoC.FileUtilities;
import AoC.day7.wires.*;
import riddarvid.aoc.days.Day;

import java.util.List;
import java.util.Map;

public class Day7 extends Day {
    public static void main(String[] args) {
        new Day7();
    }

    int resultA;

    Map<String, Wire> map;

    @Override
    protected void part1() {
        Wire a = Wire.getWire("a");
        System.out.println(a.getValue());
        resultA = a.getValue();
    }

    @Override
    protected void part2() {
        map = Wire.getMap();
        setup();
        new Set("b", Integer.toString(resultA));
        Wire a = map.get("a");
        System.out.println(a.getValue());
    }

    @Override
    protected void setup() {
        for (String instruction : lines) {
            List<String> tokens = FileUtilities.getTokens(instruction, ' ');
            if (tokens.size() == 3) {
                new Set(tokens.get(2), tokens.get(0));
            } else if (tokens.size() == 4) {
                new Not(tokens.get(3), tokens.get(1));
            } else {
                String id = tokens.get(4);
                String o1 = tokens.get(0);
                String o2 = tokens.get(2);
                switch (tokens.get(1)) {
                    case "AND":
                        new And(id, o1, o2);
                        break;
                    case "OR":
                        new Or(id, o1, o2);
                        break;
                    case "LSHIFT":
                        new Lshift(id, o1, o2);
                        break;
                    case "RSHIFT":
                        new Rshift(id, o1, o2);
                        break;
                    default:
                        System.out.println("Unsupported operation");
                }
            }
        }
        map = Wire.getMap();
    }
}

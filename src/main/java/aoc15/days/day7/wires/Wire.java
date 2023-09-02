package aoc15.days.day7.wires;

import java.util.HashMap;
import java.util.Map;

public abstract class Wire {
    static Map<String, Wire> wires = new HashMap<>();

    protected String id;
    protected boolean calculated = false;

    public Wire(String id) {
        this.id = id;
        wires.put(id, this);
    }

    public int getValue() {
        if (!calculated) {
            calculateInputs();
            calculated = true;
        }
        return getValueSpecific();
    }

    public static void reset() {
        wires = new HashMap<>();
    }

    abstract int getValueSpecific();

    abstract void calculateInputs();

    public static void putWire(Wire wire) {
        wires.put(wire.id, wire);
    }

    public static Wire getWire(String key) {
        return wires.get(key);
    }

    public static Map<String, Wire> getMap() {
        return wires;
    }
}

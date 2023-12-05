package aoc.days.Day11;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

class Puzzle21Test {

    @org.junit.Test
    void initPowerLevels() {
        assertEquals(-5, Puzzle21.initPowerLevels(57, 300)[121][78]);
        assertEquals(0, Puzzle21.initPowerLevels(39, 300)[216][195]);
        assertEquals(4, Puzzle21.initPowerLevels(71, 300)[100][152]);
    }

    @Test
    void calculateValue() {
        assertEquals(29, Puzzle21.calculateValue(Puzzle21.initPowerLevels(18, 300), 3, 32, 44));
    }

    @Test
    void getMaxEntry() {
        int[][] powerLevels = Puzzle21.initPowerLevels(18, 300);
        int[][] values = Puzzle21.calculateValues(powerLevels, 3);
        Entry max = Puzzle21.getMaxEntry(values, 3);
        assertEquals(true, new Entry(33, 45, 3, 29).equals(max));
    }
}
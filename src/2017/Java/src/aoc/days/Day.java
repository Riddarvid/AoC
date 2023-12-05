package aoc.days;

import aoc.utils.InputUtilities;

import java.io.File;
import java.net.URL;
import java.util.List;

public abstract class Day {
    protected List<String> lines;

    public Day() {
        URL url = getClass().getResource("input");
        File f = new File(url.getPath());
        lines = InputUtilities.getLines(f);
        setup();
        part1();
        part2();
    }

    protected abstract void part1();

    protected abstract void part2();

    protected abstract void setup();
}

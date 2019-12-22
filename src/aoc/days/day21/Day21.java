package aoc.days.day21;

import aoc.days.Day;
import aoc.utils.input.InputUtils;
import aoc.utils.intcode.Communicator;

import java.io.File;
import java.net.URL;

public class Day21 extends Day {
    private long[] memory;
    private char[] springScript1;
    private char[] springScript2;

    public static void main(String[] args) {
        new Day21();
    }

    @Override
    protected void part1() {
        Communicator cm = new JumpCommunicator(memory);
        Thread t = new Thread(cm);
        t.start();
        inputScript(cm, springScript1);
        while (cm.isRunning());
    }

    private void inputScript(Communicator cm, char[] springScript) {
        for (char c : springScript) {
            cm.makeRequest(c);
        }
    }

    @Override
    protected void part2() {
        Communicator cm = new JumpCommunicator(memory);
        Thread t = new Thread(cm);
        t.start();
        inputScript(cm, springScript2);
    }


    @Override
    protected void setup() {
        memory = InputUtils.readProgram(lines);
        URL url = getClass().getResource("springScript1");
        File f = new File(url.getPath());
        springScript1 = InputUtils.readSpringCode(f);
        url = getClass().getResource("springScript2");
        f = new File(url.getPath());
        springScript2 = InputUtils.readSpringCode(f);
    }
}

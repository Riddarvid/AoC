package aoc.days.day13;

import aoc.FileUtilities;
import aoc.days.Day;
import aoc.days.day13.view.View;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class Day13 extends Day {
    private Map<Integer, Layer> layerMap;
    private int deepestLayer;

    public static void main(String[] args) {
        new Day13();
    }

    @Override
    protected void part1() {
        int severity = 0;
        for (int turn = 0; turn <= deepestLayer; turn++) {
            Layer current = layerMap.get(turn);
            if (current != null) {
                if (current.getScannerDepth() == 0) {
                    severity += current.getDepth() * current.getRange();
                }
            }
            moveScanners();
        }
        System.out.println(severity);
    }

    private void moveScanners() {
        for (Layer layer : layerMap.values()) {
            layer.moveScanner();
        }
    }

    @Override
    protected void part2() {
        //View view = new View(deepestLayer);
        resetScanners();
        int delay = 0;
        List<Packet> activePackets = new ArrayList<>();
        do {

            movePackets(activePackets);
            Packet current = new Packet(delay);
            delay++;
            activePackets.add(current);
            List<Packet> caught = getCaught(activePackets);
            activePackets.removeAll(caught);
            moveScanners();
            //view.update(layerMap, activePackets);
        } while (!packetFinished(activePackets));
        System.out.println(getFinishedPacket(activePackets).getDelay());
    }

    private Packet getFinishedPacket(List<Packet> activePackets) {
        for (Packet packet : activePackets) {
            if (packet.getPosition() == deepestLayer) {
                return packet;
            }
        }
        return null;
    }

    private boolean packetFinished(List<Packet> activePackets) {
        for (Packet packet : activePackets) {
            if (packet.getPosition() == deepestLayer) {
                return true;
            }
        }
        return false;
    }

    private List<Packet> getCaught(List<Packet> activePackets) {
        List<Packet> caught = new ArrayList<>();
        for (Packet packet : activePackets) {
            Layer layer = layerMap.get(packet.getPosition());
            if (layer != null && layer.getScannerDepth() == 0) {
                caught.add(packet);
            }
        }
        return caught;
    }

    private void movePackets(List<Packet> activePackets) {
        for (Packet packet : activePackets) {
            packet.move();
        }
    }

    private void resetScanners() {
        for (Layer layer : layerMap.values()) {
            layer.resetScanner();
        }
    }

    @Override
    protected void setup() {
        layerMap = new HashMap<>();
        deepestLayer = FileUtilities.getInts(lines.get(0)).get(0);
        for (String s : lines) {
            List<Integer> ints = FileUtilities.getInts(s);
            Layer current = new Layer(ints.get(0), ints.get(1));
            layerMap.put(current.getDepth(), current);
            if (current.getDepth() > deepestLayer) {
                deepestLayer = current.getDepth();
            }
        }
    }
}

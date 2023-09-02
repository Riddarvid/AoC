package aoc21.days.day15;

import aoc.days.Day;
import aoc.math.geometry.Point;

import java.util.HashSet;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

public class Day15 extends Day {
    private Node startNode1;
    private Node endNode1;
    private Node startNode2;
    private Node endNode2;

    private Node[][] generateNodeTile(int tileX, int tileY) {
        Node[][] nodes = new Node[lines.size()][lines.get(0).length()];
        int increase = tileX + tileY;
        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines.get(0).length(); x++) {
                int riskLevel = lines.get(y).charAt(x) - '0';
                riskLevel += increase;
                while (riskLevel > 9) {
                    riskLevel -= 9;
                }
                nodes[y][x] = new Node(new Point(x, y), riskLevel);
            }
        }
        return nodes;
    }

    private void generateConnections(Node[][] nodes) {
        for (int y = 0; y < nodes.length; y++) {
            for (int x = 0; x < nodes[0].length; x++) {
                Node current = nodes[y][x];
                if (x - 1 >= 0) {
                    current.addNeighbour(nodes[y][x - 1]);
                }
                if (x + 1 < nodes[0].length) {
                    current.addNeighbour(nodes[y][x + 1]);
                }
                if (y - 1 >= 0) {
                    current.addNeighbour(nodes[y - 1][x]);
                }
                if (y + 1 < nodes.length) {
                    current.addNeighbour(nodes[y + 1][x]);
                }
            }
        }
    }

    private void generateGraph1() {
        Node[][] nodes = generateNodeTile(0, 0);
        generateConnections(nodes);
        startNode1 = nodes[0][0];
        endNode1 = nodes[nodes.length - 1][nodes[0].length - 1];
    }

    private void insertMatrix(Node[][] targetMatrix, Node[][] tile, int xOffset, int yOffset) {
        for (int x = 0; x < tile[0].length; x++) {
            for (int y = 0; y < tile.length; y++) {
                targetMatrix[yOffset + y][xOffset + x] = tile[y][x];
            }
        }
    }

    private void generateGraph2() {
        Node[][] nodes = new Node[lines.size() * 5][lines.get(0).length() * 5];
        for (int tileX = 0; tileX < 5; tileX++) {
            for (int tileY = 0; tileY < 5; tileY++) {
                Node[][] tile = generateNodeTile(tileX, tileY);
                insertMatrix(nodes, tile, tileX * tile[0].length, tileY * tile.length);
            }
        }
        generateConnections(nodes);
        startNode2 = nodes[0][0];
        endNode2 = nodes[nodes.length - 1][nodes[0].length - 1];
    }

    @Override
    public void setup() {
        generateGraph1();
        generateGraph2();
    }

    private void dijkstra(Node startNode, Node endNode) {
        Set<Node> determined = new HashSet<>();
        Queue<Node> pq = new PriorityQueue<>();
        startNode.setTotalRiskLevel(0);
        pq.add(startNode);
        while (!pq.peek().equals(endNode)) {
            Node current = pq.poll();
            determined.add(current);
            Set<Node> neighbours = current.getNeighbours();
            for (Node neighbour : neighbours) {
                if (!determined.contains(neighbour)) {
                    long newRiskLevel = current.getTotalRiskLevel() + neighbour.getRiskLevel();
                    neighbour.updateTotalRiskLevel(newRiskLevel, current);
                    if (!pq.contains(neighbour)) {
                        pq.add(neighbour);
                    }
                }
            }
        }
    }

    @Override
    public long part1() {
        dijkstra(startNode1, endNode1);
        return endNode1.getTotalRiskLevel();
    }

    @Override
    public long part2() {
        dijkstra(startNode2, endNode2);
        return endNode2.getTotalRiskLevel();
    }
}

package aoc.days.Day22;

import java.util.PriorityQueue;

public class Day22 {
    Region[][] regions;
    Region[][] regionsOtherTool;
    Region target;
    int depth = 11991;
    int targetX = 6;
    int targetY = 797;
    static int INFINITY = Integer.MAX_VALUE;

    boolean paintMap = true;
    boolean paintPath = false;
    boolean paintDistance = false;

    public static void main(String[] args) {
        new Day22();
    }

    public Day22() {

        regions = new Region[1000][1000];
        for (int row = 0; row < regions.length; row++) {
            for (int col = 0; col < regions[row].length; col++) {
                regions[row][col] = generateRegion(col, row, regions, depth);
            }
        }
        target = regions[targetY][targetX];
        Region start = regions[0][0];
        RegionView view = new RegionView(this);
        view.update();
        regionsOtherTool = new Region[regions.length][regions[0].length];
        for (int row = 0; row < regions.length; row++) {
            for (int col = 0; col < regions[row].length; col++) {
                Region r = regions[row][col];
                Region copy = r.getCopyWithOtherTool();
                regionsOtherTool[row][col] = copy;
            }
        }
        dijkstra(start, target, regions, regionsOtherTool);
        //paintMap = false;
        paintPath = true;
        //paintDistance = true;
        view.update();
        printPath(target);
        System.out.println(target.getDistance());
    }

    private void printPath(Region target) {
        Region r = target;
        while (r.getPrevious() != null) {
            System.out.println(r);
            r = r.getPrevious();
        }
        System.out.println(r);
    }

    private Region generateRegion(int col, int row, Region[][] map, int depth) {
        if (col == 0 && row == 0) {
            return new Region(0, col, row, depth);
        } else if (col == targetX && row == targetY) {
            return new Region(0, col, row, depth);
        } else if (row == 0) {
            return new Region(16807 * col, col, row, depth);
        } else if (col == 0) {
            return new Region(48271 * row, col, row, depth);
        } else {
            return new Region(map[row - 1][col].getErosionLevel() * map[row][col - 1].getErosionLevel(), col, row, depth);
        }
    }

    private void printMatrix(Region[][] matrix) {
        for (int row = 0; row < matrix.length; row++) {
            System.out.println();
            for (int col = 0; col < matrix[row].length; col++) {
                if (matrix[row][col].getRiskLevel() == 0) {
                    System.out.print(".");
                } else if (matrix[row][col].getRiskLevel() == 1) {
                    System.out.print("=");
                } else if (matrix[row][col].getRiskLevel() == 2) {
                    System.out.print("|");
                } else {
                    System.out.print("?");
                }
            }
        }
    }

    private int sumOfMatrix(Region[][] matrix) {
        int sum = 0;
        for (int row = 0; row < matrix.length; row++) {
            for (int col = 0; col < matrix[row].length; col++) {
                sum += matrix[row][col].getRiskLevel();
            }
        }
        return sum;
    }

    private void dijkstra(Region start, Region target, Region[][] map, Region[][] copyMap) {
        start.setDistance(0);
        PriorityQueue<Region> pq = new PriorityQueue<>();
        pq.add(start);
        while (!pq.isEmpty()) {
            Region w = pq.poll();
            if (!w.visited) {
                if (w == target) {
                    return;
                }
                w.visited = true;
                examineNeighbours(w, map, copyMap, pq);
            }
        }
    }

    public void examineNeighbours(Region w, Region[][] map, Region[][] copyMap, PriorityQueue<Region> pq) {
        int x = w.getX();
        int y = w.getY();
        Region v;
        if (map[y][x] == w) {
            v = copyMap[y][x];
        } else {
            v = map[y][x];
        }
        updateCost(w, v, pq);
        if (x > 0) {
            v = map[w.getY()][w.getX() - 1];
            updateCost(w, v, pq);
            v = copyMap[w.getY()][w.getX() - 1];
            updateCost(w, v, pq);
        }
        if (x < map[0].length - 1) {
            v = map[w.getY()][w.getX() + 1];
            updateCost(w, v, pq);
            v = copyMap[w.getY()][w.getX() + 1];
            updateCost(w, v, pq);
        }
        if (y > 0) {
            v = map[w.getY() - 1][w.getX()];
            updateCost(w, v, pq);
            v = copyMap[w.getY() - 1][w.getX()];
            updateCost(w, v, pq);
        }
        if (y < map.length - 1) {
            v = map[w.getY() + 1][w.getX()];
            updateCost(w, v, pq);
            v = copyMap[w.getY() + 1][w.getX()];
            updateCost(w, v, pq);
        }
    }

    private void updateCost(Region w, Region v, PriorityQueue<Region> pq) {
        if (w.getDistance() + cost(w, v) < v.getDistance()) {
            v.setDistance(w.getDistance() + cost(w, v));
            v.setPrevious(w);
        }
        pq.add(v);
    }

    private int cost(Region a, Region b) {
        int ax = a.getX();
        int ay = a.getY();
        int bx = b.getX();
        int by = b.getY();
        int sum = 0;
        switch (a.getRiskLevel()) {
            case 0:
                if (b.getTool() == Tool.NONE) {
                    return INFINITY;
                }
                break;
            case 1:
                if (b.getTool() == Tool.TORCH) {
                    return INFINITY;
                }
                break;
            case 2:
                if (b.getTool() == Tool.CLIMB) {
                    return INFINITY;
                }
                break;
        }
        if (a.getTool() != b.getTool()) {
            sum += 7;
        }
        if (ax == bx && ay == by) {
            return sum;
        }
        if (ax == bx) {
            if (ay == by + 1 || ay == by - 1) {
                return sum + 1;
            }
        }
        if (ay == by) {
            if (ax == bx + 1 || ax == bx - 1) {
                return sum + 1;
            }
        }
        return INFINITY;
    }
}

package days.day20;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.math.geometry.Point;
import riddarvid.aoc.parsing.ParsingUtils;
import riddarvid.aoc.printing.PrintUtils;

import java.util.*;

public class Day20 extends Day {
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_RED = "\u001B[31m";

    private List<Tile> tiles;
    private int tilesPerSide;
    private int squaresPerSide;
    private char[][] seaMonster;

    public static void main(String[] args) {
        new Day20().runAndPrint();
    }

    @Override
    public long part1() {
        List<Tile> corners = getCornerTiles();
        long product = 1;
        for (Tile corner : corners) {
            product *= corner.getId();
        }
        return product;
    }

    private List<Tile> getCornerTiles() {
        List<Tile> corners = new ArrayList<>();
        for (Tile tile : tiles) {
            if (tile.isCornerTile(tiles)) {
                corners.add(tile);
            }
        }
        return corners;
    }

    @Override
    public long part2() {
        Map<Tile, Point> tileMap = getTileMap();
        char[][] image = getImage(tileMap);
        Tile tile = new Tile(image);
        tile = tile.getCorrectRotation(seaMonster);
        List<Point> seaMonsterPoints = tile.getSeaMonsterPoints(seaMonster);
        printSeaMonsters(tile.getImage(), seaMonsterPoints);
        return tile.getTotalRoughness() - seaMonsterPoints.size() * 15;
    }

    private void printSeaMonsters(char[][] image, List<Point> seaMonsterPoints) {
        Set<Point> allMonsterPoints = new HashSet<>();
        for (Point point : seaMonsterPoints) {
            allMonsterPoints.addAll(getAllMonsterPoints(point));
        }
        for (int y = 0; y < image.length; y++) {
            for (int x = 0; x < image[y].length; x++) {
                if (allMonsterPoints.contains(new Point(x, y))) {
                    System.out.print(ANSI_RED + image[y][x] + ANSI_RESET);
                } else {
                    System.out.print(image[y][x]);
                }
            }
            System.out.println();
        }
    }

    private Set<Point> getAllMonsterPoints(Point point) {
        Set<Point> allPoints = new HashSet<>();
        for (int y = 0; y < seaMonster.length; y++) {
            for (int x = 0; x < seaMonster[y].length; x++) {
                if (seaMonster[y][x] == '#') {
                    allPoints.add(point.moveBy(x, y));
                }
            }
        }
        return allPoints;
    }

    private char[][] getImage(Map<Tile, Point> tileMap) {
        Map<Point, Tile> pointMap = invertMap(tileMap);
        char[][] image = new char[tilesPerSide * squaresPerSide][];
        for (int i = 0; i < image.length; i++) {
            image[i] = new char[tilesPerSide * squaresPerSide];
        }
        Point topLeft = tileMap.get(tiles.get(0));
        for (Point point : pointMap.keySet()) {
            if (point.getX() < topLeft.getX() || point.getY() > topLeft.getY()) {
                topLeft = point;
            }
        }
        for (int y = 0; y < tilesPerSide; y++) {
            for (int x = 0; x < tilesPerSide; x++) {
                Point point = topLeft.moveBy(x, -y);
                Tile tile = pointMap.get(point);
                assembleImage(image, tile, x, y);
            }
        }
        return image;
    }

    private void assembleImage(char[][] image, Tile tile, int offsetX, int offsetY) {
        char[][] tileImage = tile.getImage();
        for (int y = 0; y < squaresPerSide; y++) {
            System.arraycopy(tileImage[y + 1], 1, image[offsetY * squaresPerSide + y], offsetX * squaresPerSide, squaresPerSide);
        }
    }

    private Map<Point, Tile> invertMap(Map<Tile, Point> tileMap) {
        Map<Point, Tile> inverted = new HashMap<>();
        for (Tile tile : tileMap.keySet()) {
            inverted.put(tileMap.get(tile), tile);
        }
        return inverted;
    }

    private Map<Tile, Point> getTileMap() {
        Set<Tile> remaining = new HashSet<>(tiles);
        Set<Tile> foundLastRound = new HashSet<>();
        remaining.remove(tiles.get(0));
        foundLastRound.add(tiles.get(0));
        Map<Tile, Point> tileMap = new HashMap<>();
        tileMap.put(tiles.get(0), new Point(0, 0));
        while (!foundLastRound.isEmpty()) {
            Set<Tile> newFoundLastRound = new HashSet<>();
            for (Tile current : foundLastRound) {
                Point currentPoint = tileMap.get(current);
                for (Direction direction : Direction.values()) {
                    Point point;
                    switch (direction) {
                        case NORTH -> point = currentPoint.moveBy(0, 1);
                        case EAST -> point = currentPoint.moveBy(1, 0);
                        case SOUTH -> point = currentPoint.moveBy(0, -1);
                        case WEST -> point = currentPoint.moveBy(-1, 0);
                        default -> throw new InputMismatchException();
                    }
                    if (!tileMap.containsValue(point)) {
                        Tile match = current.getMatch(direction, remaining);
                        if (match == null) {
                            continue;
                        }
                        remaining.remove(match);
                        Tile matchCorrectRotation = current.getCorrectRotation(match, direction);
                        tileMap.put(matchCorrectRotation, point);
                        newFoundLastRound.add(matchCorrectRotation);
                    }
                }
            }
            foundLastRound = newFoundLastRound;
        }
        return tileMap;
    }

    @Override
    public void setup() {
        tiles = new ArrayList<>();
        squaresPerSide = 0;
        while (!lines.get(squaresPerSide).equals("")) {
            squaresPerSide++;
        }
        squaresPerSide -= 3;
        for (int i = 0; i < lines.size(); i += squaresPerSide + 4) {
            tiles.add(parseTile(i));
        }
        tilesPerSide = (int)Math.sqrt(tiles.size());
        seaMonster = new char[3][];
        seaMonster[0] = "                  # ".toCharArray();
        seaMonster[1] = "#    ##    ##    ###".toCharArray();
        seaMonster[2] = " #  #  #  #  #  #   ".toCharArray();
    }

    private Tile parseTile(int i) {
        int id = ParsingUtils.getIntegers(lines.get(i)).get(0);
        char[][] image = new char[10][];
        for (int j = 1; j <= 10; j++) {
            image[j - 1] = lines.get(i + j).toCharArray();
        }
        return new Tile(id, image);
    }
}

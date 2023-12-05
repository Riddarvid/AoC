package aoc.days.Day17;

import java.util.List;
import java.util.Objects;

public class Water {
    static int maxY = 1700;

    int x;
    int y;
    boolean still = false;

    public Water(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public void flow(char[][] map, List<Water> toBeAdded) {
        if (!still && y < maxY - 1) {
            if (map[y + 1][x] == '.') {
                map[y + 1][x] = '|';
                toBeAdded.add(new Water(x, y + 1));
            } else if (map[y + 1][x] == '#' || map[y + 1][x] == '~') {
                if (map[y][x - 1] == '.') {
                    map[y][x - 1] = '|';
                    toBeAdded.add(new Water(x - 1, y));
                }
                if (map[y][x + 1] == '.') {
                    map[y][x + 1] = '|';
                    toBeAdded.add(new Water(x + 1, y));
                }
            }
        }
    }

    public void makeStill(char[][] map, List<Water> waterList) {
        if (!still) {
            if (shouldBeStill(map)) {
                makeLeftStill(map, waterList,  x - 1, y);
                makeRightStill(map, waterList, x, y);
            }
        }
    }

    private boolean shouldBeStill(char[][] map) {
        int tempX = x;
        if (y >= 1700 - 1) {
            return false;
        }
        while (map[y][tempX] == '|') {
            if (map[y + 1][tempX] != '#' && map[y + 1][tempX] != '~') {
                return false;
            }
            tempX--;
        }
        if (map[y][tempX] != '#') {
            return false;
        }
        tempX = x;
        while (map[y][tempX] == '|') {
            if (map[y + 1][tempX] != '#' && map[y + 1][tempX] != '~') {
                return false;
            }
            tempX++;
        }
        if (map[y][tempX] != '#') {
            return false;
        }
        return true;
    }

    private void makeLeftStill(char[][] map, List<Water> waterList, int x, int y) {
        if (map[y][x] != '#') {
            map[y][x] = '~';
            waterList.get(waterList.indexOf(new Water(x, y))).still = true;
            makeLeftStill(map, waterList, x - 1, y);
        }
    }

    private void makeRightStill(char[][] map, List<Water> waterList, int x, int y) {
        if (map[y][x] != '#') {
            map[y][x] = '~';
            waterList.get(waterList.indexOf(new Water(x, y))).still = true;
            makeRightStill(map, waterList, x + 1, y);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Water water = (Water) o;
        return x == water.x &&
                y == water.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}

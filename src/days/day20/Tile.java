package days.day20;

import riddarvid.aoc.math.geometry.Point;

import java.util.*;

public class Tile {

    private final int id;
    private final char[][] image;

    public Tile(int id, char[][] image) {
        this.id = id;
        this.image = image;
    }

    public Tile(char[][] image) {
        this(0, image);
    }

    public Tile getMatch(Direction direction, Collection<Tile> allTiles) {
        List<Tile> otherTiles = new ArrayList<>(allTiles);
        otherTiles.remove(this);
        for (Tile tile : otherTiles) {
            if (matches(direction, tile)) {
                return tile;
            }
        }
        return null;
    }

    public List<Tile> getMatches(Direction direction, List<Tile> allTiles) {
        List<Tile> otherTiles = new ArrayList<>(allTiles);
        otherTiles.remove(this);
        List<Tile> matches = new ArrayList<>();
        for (Tile tile : otherTiles) {
            if (matches(direction, tile)) {
                matches.add(tile);
            }
        }
        return matches;
    }

    private boolean matches(Direction directionThis, Tile tile) {
        char[] border = getBorder(directionThis, false);
        for (Direction directionOther : Direction.values()) {
            if (Arrays.equals(border, tile.getBorder(directionOther, false))) {
                return true;
            }
        }
        for (Direction directionOther : Direction.values()) {
            if (Arrays.equals(border, tile.getBorder(directionOther, true))) {
                return true;
            }
        }
        return false;
    }

    private char[] getBorder(Direction direction, boolean reversed) {
        char[] border;
        switch (direction) {
            case NORTH -> border = getNorthBorder(reversed);
            case EAST -> border = getEastBorder(reversed);
            case SOUTH -> border = getSouthBorder(reversed);
            case WEST -> border = getWestBorder(reversed);
            default -> throw new IllegalArgumentException();
        }
        return border;
    }

    private char[] getNorthBorder(boolean reversed) {
        char[] border;
        if (reversed) {
            border = new char[image.length];
            for (int i = 0; i < image.length; i++) {
                border[i] = image[0][image.length - 1 - i];
            }
        } else {
            border = image[0];
        }
        return border;
    }

    private char[] getEastBorder(boolean reversed) {
        char[] border = new char[image.length];
        if (reversed) {
            for (int i = 0; i < image.length; i++) {
                border[i] = image[image.length - 1 - i][image.length - 1];
            }
        } else {
            for (int i = 0; i < image.length; i++) {
                border[i] = image[i][image.length - 1];
            }
        }
        return border;
    }

    private char[] getSouthBorder(boolean reversed) {
        char[] border;
        if (reversed) {
            border = image[image.length - 1];
        } else {
            border = new char[image.length];
            for (int i = 0; i < image.length; i++) {
                border[i] = image[image.length - 1][image.length - 1 - i];
            }
        }
        return border;
    }

    private char[] getWestBorder(boolean reversed) {
        char[] border = new char[image.length];
        if (reversed) {
            for (int i = 0; i < image.length; i++) {
                border[i] = image[i][0];
            }
        } else {
            for (int i = 0; i < image.length; i++) {
                border[i] = image[image.length - 1 - i][0];
            }
        }
        return border;
    }

    public int getId() {
        return id;
    }

    public boolean isCornerTile(List<Tile> allTiles) {
        int matches = 0;
        for (Direction direction : Direction.values()) {
            matches += getMatches(direction, allTiles).size();
        }
        return matches == 2;
    }

    public Tile getCorrectRotation(char[][] seaMonster) {
        Tile correct = this;
        for (int rotations = 0; rotations < 4; rotations++) {
            if (correct.containsSeaMonster(seaMonster)) {
                return correct;
            }
            correct = correct.rotate(1);
        }
        correct.flipHorizontal();
        for (int rotations = 0; rotations < 4; rotations++) {
            if (correct.containsSeaMonster(seaMonster)) {
                return correct;
            }
            correct = correct.rotate(1);
        }
        throw new InputMismatchException("No sea monster found");
    }

    private boolean containsSeaMonster(char[][] seaMonster) {
        for (int y = 0; y < image.length - seaMonster.length; y++) {
            for (int x = 0; x < image[y].length - seaMonster[0].length; x++) {
                if (hasSeaMonster(seaMonster, x, y)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean hasSeaMonster(char[][] seaMonster, int x, int y) {
        for (int row = 0; row < seaMonster.length; row++) {
            for (int col = 0; col < seaMonster[0].length; col++) {
                if (seaMonster[row][col] == '#' && image[y + row][x + col] != '#') {
                    return false;
                }
            }
        }
        return true;
    }

    public Tile getCorrectRotation(Tile match, Direction directionThis) {
        char[] border = getBorder(directionThis, false);
        for (Direction directionOther : Direction.values()) {
            if (Arrays.equals(border, match.getBorder(directionOther, false))) {
                return match.getRotation((directionThis.ordinal() - directionOther.ordinal() + 6) % 4, true, directionThis);
            }
        }
        for (Direction directionOther : Direction.values()) {
            if (Arrays.equals(border, match.getBorder(directionOther, true))) {
                return match.getRotation((directionThis.ordinal() - directionOther.ordinal() + 6) % 4, false, directionThis);
            }
        }
        throw new InputMismatchException();
    }

    private Tile getRotation(int rightRotations, boolean flipped, Direction flipAxis) {
        Tile rotated = rotate(rightRotations);
        if (flipped) {
            rotated.flip(flipAxis);
        }
        return rotated;
    }

    private void flip(Direction flipAxis) {
        if (flipAxis == Direction.NORTH || flipAxis == Direction.SOUTH) {
            flipHorizontal();
        } else {
            flipVertical();
        }
    }

    private void flipVertical() {
        for (int i = 0; i < image.length / 2; i++) {
            for (int j = 0; j < image.length; j++) {
                char temp = image[i][j];
                image[i][j] = image[image.length - 1 - i][j];
                image[image.length - 1 - i][j] = temp;
            }
        }
    }

    private void flipHorizontal() {
        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image.length / 2; j++) {
                char temp = image[i][j];
                image[i][j] = image[i][image.length - 1 - j];
                image[i][image.length - 1 - j] = temp;
            }
        }
    }

    private Tile rotate(int rightRotations) {
        char[][] imageCopy = new char[image.length][];
        for (int i = 0; i < image.length; i++) {
            imageCopy[i] = Arrays.copyOf(image[i], image.length);
        }
        while (rightRotations > 0) {
            imageCopy = rotateOnce(imageCopy);
            rightRotations--;
        }
        return new Tile(id, imageCopy);
    }

    private char[][] rotateOnce(char[][] imageCopy) {
        char[][] rotated = new char[image.length][];
        for (int i = 0; i < image.length; i++) {
            rotated[i] = new char[image.length];
        }
        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image.length; j++) {
                rotated[j][image.length - 1 - i] = imageCopy[i][j];
            }
        }
        return rotated;
    }

    @Override
    public String toString() {
        return "Id: " + id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Tile tile = (Tile) o;
        return id == tile.id && Arrays.equals(image, tile.image);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(id);
        result = 31 * result + Arrays.hashCode(image);
        return result;
    }

    public char[][] getImage() {
        return image;
    }

    public List<Point> getSeaMonsterPoints(char[][] seaMonster) {
        List<Point> seaMonsterPoints = new ArrayList<>();
        for (int y = 0; y < image.length - seaMonster.length; y++) {
            for (int x = 0; x < image[y].length - seaMonster[0].length; x++) {
                if (hasSeaMonster(seaMonster, x, y)) {
                    seaMonsterPoints.add(new Point(x, y));
                }
            }
        }
        return seaMonsterPoints;
    }

    public long getTotalRoughness() {
        long roughness = 0;
        for (int y = 0; y < image.length; y++) {
            for (int x = 0; x < image[y].length; x++) {
                if (image[y][x] == '#') {
                    roughness++;
                }
            }
        }
        return roughness;
    }
}

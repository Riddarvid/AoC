package AoC.Day11;

import java.util.Objects;

public class Entry {
    private int x;
    private int y;
    private int size;
    private int value;

    public Entry(int x, int y, int size, int value) {
        this.x = x;
        this.y = y;
        this.size = size;
        this.value = value;
    }

    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Entry entry = (Entry) o;
        return x == entry.x &&
                y == entry.y &&
                size == entry.size &&
                value == entry.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, size, value);
    }

    @Override
    public String toString() {
        return "Entry{" +
                "x=" + x +
                ", y=" + y +
                ", size=" + size +
                ", value=" + value +
                '}';
    }
}

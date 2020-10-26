package aoc.days.Day13;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Cart implements Comparable<Cart> {
    private static char[][] map;
    private static List<Cart> carts = new ArrayList<>();

    private int x;
    private int y;
    private boolean toBeRemoved;
    private AbsoluteDirection direction;
    private int turnIndex = 0;

    public Cart(int x, int y, AbsoluteDirection direction, char[][] map) {
        if (Cart.map == null) {
            Cart.map = map;
        }
        this.x = x;
        this.y = y;
        this.direction = direction;
    }

    @Override
    public String toString() {
        return "Cart{" +
                "x=" + x +
                ", y=" + y +
                ", direction=" + direction +
                ", turnIndex=" + turnIndex +
                '}';
    }

    public void update() {
        switch (direction) {
            case NORTH:
                y--;
                break;
            case EAST:
                x++;
                break;
            case SOUTH:
                y++;
                break;
            case WEST:
                x--;
        }
        if (map[y][x] == '/') {
            if (direction == AbsoluteDirection.NORTH || direction == AbsoluteDirection.SOUTH) {
                turnRight();
            } else {
                turnLeft();
            }
        } else if (map[y][x] == '\\') {
            if (direction == AbsoluteDirection.NORTH || direction == AbsoluteDirection.SOUTH) {
                turnLeft();
            } else {
                turnRight();
            }
        } else if (map[y][x] == '+') {
            if (turnIndex == 0) {
                turnLeft();
            } else if (turnIndex == 2) {
                turnRight();
            }
            turnIndex = (turnIndex + 1) % 3;
        }
        Cart c;
        if ((c = hasColided()) != null) {
            System.out.println(this.toString());
            toBeRemoved = true;
            c.toBeRemoved = true;
        }
    }

    public void turnLeft() {
        switch (direction) {
            case NORTH:
                direction = AbsoluteDirection.WEST;
                break;
            case EAST:
                direction = AbsoluteDirection.NORTH;
                break;
            case SOUTH:
                direction = AbsoluteDirection.EAST;
                break;
            case WEST:
                direction = AbsoluteDirection.SOUTH;
        }
    }

    public void turnRight() {
        switch (direction) {
            case NORTH:
                direction = AbsoluteDirection.EAST;
                break;
            case EAST:
                direction = AbsoluteDirection.SOUTH;
                break;
            case SOUTH:
                direction = AbsoluteDirection.WEST;
                break;
            case WEST:
                direction = AbsoluteDirection.NORTH;
        }
    }

    public Cart hasColided() {
        for (Cart c : carts) {
            if (c.equals(this) && c != this) {
                return c;
            }
        }
        return null;
    }

    @Override
    public int compareTo(Cart o) {
        return (y * 100 + x) - (o.y * 100 + o.x);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Cart cart = (Cart) o;
        return x == cart.x &&
                y == cart.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public static void setCarts(List<Cart> carts) {
        Cart.carts = carts;
    }

    public boolean isToBeRemoved() {
        return toBeRemoved;
    }
}

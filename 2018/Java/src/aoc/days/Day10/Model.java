package aoc.days.Day10;

import aoc.RegEx;

import java.util.ArrayList;
import java.util.List;

public class Model {
    private final Point[] points;
    private final List<Observer> observers = new ArrayList<>();
    private boolean forward = true;
    private boolean paused = false;
    private int time = 0;

    public void pause() {
        paused = !paused;
        System.out.println(time);
    }

    public Model(String[] input) {
        points = initPoints(input);
    }

    protected int getMinX() {
        int min = points[0].getX();
        for (Point p : points) {
            if (p.getX() < min) {
                min = p.getX();
            }
        }
        return min;
    }

    protected int getMaxX() {
        int max = points[0].getX();
        for (Point p : points) {
            if (p.getX() > max) {
                max = p.getX();
            }
        }
        return max;
    }

    protected int getMinY() {
        int min = points[0].getY();
        for (Point p : points) {
            if (p.getY() < min) {
                min = p.getY();
            }
        }
        return min;
    }

    protected int getMaxY() {
        int max = points[0].getY();
        for (Point p : points) {
            if (p.getY() > max) {
                max = p.getY();
            }
        }
        return max;
    }


    protected void changeDirection() {
        forward = !forward;
    }

    private Point[] initPoints(String[] input) {
        List<Integer>[] data = new List[input.length];
        for (int i = 0; i < data.length; i++) {
            data[i] = RegEx.getIntegers(input[i]);
        }
        Point[] points = new Point[data.length];
        for (int i = 0; i < data.length; i++) {
            List<Integer> current = data[i];
            points[i] = new Point(current.get(0), current.get(1), current.get(2), current.get(3));
        }
        return points;
    }

    public void updateForwards() {
        if (!paused) {
            for (Point p : points) {
                p.move();
            }
            time++;
        }
    }

    public void updateBackwards() {
        if (!paused) {
            for (Point p : points) {
                p.moveBackwards();
            }
            time--;
        }
    }

    public void run() {
        while (true) {
            updateForwards();
        }
    }

    private void notifyObservers() {
        for (Observer o : observers) {
            o.act();
        }
    }

    public void addObserver(Observer observer) {
        observers.add(observer);
    }

    public Point[] getPoints() {
        return points;
    }
}

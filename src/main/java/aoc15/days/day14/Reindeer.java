package aoc15.days.day14;

public class Reindeer {
    private final String name;
    private boolean isResting = false;
    private final int speed;
    private final int totalRestTime;
    private final int totalFlightTime;
    private int currentRestTime = 0;
    private int currentFlightTime = 0;
    private int distanceTraveled = 0;
    private int points = 0;

    public Reindeer(String name, int speed, int totalFlightTime, int totalRestTime) {
        this.name = name;
        this.speed = speed;
        this.totalRestTime = totalRestTime;
        this.totalFlightTime = totalFlightTime;
    }

    public void move() {
        if (isResting) {
            currentRestTime++;
            if (currentRestTime == totalRestTime) {
                isResting = false;
                currentRestTime = 0;
            }
        } else {
            distanceTraveled += speed;
            currentFlightTime++;
            if (currentFlightTime == totalFlightTime) {
                isResting = true;
                currentFlightTime = 0;
            }
        }
    }

    public int getDistanceTraveled() {
        return distanceTraveled;
    }

    public void incPoints() {
        points++;
    }

    public int getPoints() {
        return points;
    }

    public void reset() {
        isResting = false;
        currentRestTime = 0;
        currentFlightTime = 0;
        distanceTraveled = 0;
        points = 0;
    }
}

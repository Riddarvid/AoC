package AoC.Day4;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Guard implements Comparable<Guard> {
    private int[] timeAsleep= new int[60];
    private int id;

    public Guard(int id) {
        this.id = id;
    }

    public void addSleep(PointInTime a, PointInTime b) {
        int start = a.getMinute();
        int end = b.getMinute();
        while (start < end) {
            timeAsleep[start]++;
            start++;
        }
    }

    public void addSleep(List<PointInTime> times) {
        for (int i = 0; i < times.size(); i += 2) {
            addSleep(times.get(i), times.get(i + 1));
        }
    }

    public int[] getTimeAsleep() {
        return timeAsleep;
    }

    public int getId() {
        return id;
    }

    public int totalTimeAsleep() {
        int sum = 0;
        for (int i : timeAsleep) {
            sum += i;
        }
        return sum;
    }

    public int sleepiestMinute() {
        int sleepIndex = 0;
        for (int i = 0; i < timeAsleep.length; i++) {
            if (timeAsleep[i] > timeAsleep[sleepIndex]) {
                sleepIndex = i;
            }
        }
        return sleepIndex;
    }

    public int mostSleep() {
        return timeAsleep[sleepiestMinute()];
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Guard guard = (Guard) o;
        return id == guard.id;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    /*@Override
    public int compareTo(Guard o) {
        return o.totalTimeAsleep() - totalTimeAsleep();
    }*/

    @Override
    public int compareTo(Guard o) {
        return o.mostSleep() - mostSleep();
    }

    @Override
    public String toString() {
        return "{" +
                "id: " + id +
                " Sleepiest minute: " + sleepiestMinute() +
                " Total time asleep: " + totalTimeAsleep() +
                '}';
    }

    /*@Override
    public String toString() {
        return "{" + mostSleep() +
                "}";
    }*/
}

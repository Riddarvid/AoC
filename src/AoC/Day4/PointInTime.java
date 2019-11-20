package AoC.Day4;

public class PointInTime implements Comparable<PointInTime> {
    private final int year;
    private final int month;
    private final int day;
    private final int hour;
    private final int minute;

    private final static int MINUTES_IN_HOUR = 60;
    private final static int HOURS_IN_DAY = 24;

    public PointInTime(int year, int month, int day, int hour, int minute) {
        this.year = year;
        this.month = month;
        this.day = day;
        this.hour = hour;
        this.minute = minute;
    }

    public int getYear() {
        return year;
    }

    public int getMonth() {
        return month;
    }

    public int getDay() {
        return day;
    }

    public int getHour() {
        return hour;
    }

    public int getMinute() {
        return minute;
    }

    public int minutesSinceZero() {
        int sum = 0;
        for (int i = 0; i < year; i++) {
            sum += daysInYear(i) * HOURS_IN_DAY * MINUTES_IN_HOUR;
        }
        for (int i = 1; i < month; i++) {
            sum += daysInMonth(i, year) * HOURS_IN_DAY * MINUTES_IN_HOUR;
        }
        sum += day * HOURS_IN_DAY * MINUTES_IN_HOUR;
        sum += hour * MINUTES_IN_HOUR;
        sum += minute;
        return sum;
    }

    private static int daysInMonth(int month, int year) {
        switch (month) {
            case 2:
                if (isLeapYear(year)) {
                    return 29;
                } else {
                    return 28;
                }
            case 4:
            case 6:
            case 9:
            case 11:
                return 30;
        }
        return 31;
    }

    private static int daysInYear(int year) {
        if (isLeapYear(year)) {
            return  366;
        } else {
            return  365;
        }
    }

    private static boolean isLeapYear(int year) {
        return ((year % 4 == 0) && (year % 400 != 0));
    }

    @Override
    public int compareTo(PointInTime o) {
        return minutesSinceZero() - o.minutesSinceZero();
    }
}

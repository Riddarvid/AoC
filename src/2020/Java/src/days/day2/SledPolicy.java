package days.day2;

import riddarvid.aoc.math.MathUtils;

public class SledPolicy extends Policy {
    public SledPolicy(char letter, int low, int high) {
        super(letter, low, high);
    }

    @Override
    public boolean fulfills(String password) {
        int sum = count(password);
        return MathUtils.inRange(getLow(), getHigh(), sum);
    }

    private int count(String password) {
        int sum = 0;
        for (char c : password.toCharArray()) {
            if (c == getLetter()) {
                sum++;
            }
        }
        return sum;
    }
}

package aoc.days.day16;

public class Spin implements DanceMove {
    private final int distance;

    public Spin(int distance) {
        this.distance = distance;
    }

    @Override
    public void dance(char[] dancers) {
        for (int i = 0; i < distance; i++) {
            shiftOne(dancers);
        }
    }

    private void shiftOne(char[] dancers) {
        char temp = dancers[dancers.length - 1];
        System.arraycopy(dancers, 0, dancers, 1, dancers.length - 1);
        dancers[0] = temp;
    }
}

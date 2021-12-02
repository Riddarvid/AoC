package days.day2;

public class Engine2 extends Engine {
    private long aim;

    public Engine2() {
        aim = 0;
    }

    @Override
    protected void move(Instruction instruction) {
        long distance = instruction.getDistance();
        switch (instruction.getDirection()) {
            case FORWARD -> {
                horizontalPosition += distance;
                depth += aim * distance;
            }
            case UP -> aim -= distance;
            case DOWN -> aim += distance;
        }
    }
}

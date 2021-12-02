package days.day2;

public class Engine1 extends Engine {
    @Override
    protected void move(Instruction instruction) {
        long distance = instruction.getDistance();
        switch (instruction.getDirection()) {
            case FORWARD -> horizontalPosition += distance;
            case UP -> depth -= distance;
            case DOWN -> depth += distance;
        }
    }
}

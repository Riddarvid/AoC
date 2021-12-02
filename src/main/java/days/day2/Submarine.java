package days.day2;

public class Submarine {
    private final Engine engine;

    public Submarine(Engine engine) {
        this.engine = engine;
    }

    public void move(Instruction instruction) {
        engine.move(instruction);
    }

    public long getHorizontalPosition() {
        return engine.getHorizontalPosition();
    }

    public long getDepth() {
        return engine.getDepth();
    }
}

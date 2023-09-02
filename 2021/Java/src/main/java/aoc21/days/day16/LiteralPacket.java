package aoc21.days.day16;

public class LiteralPacket extends Packet {
    private final long value;

    protected LiteralPacket(int version, int typeID, long length, long value) {
        super(version, typeID, length);
        this.value = value;
        if (typeID != 4) {
            throw new IllegalArgumentException("TypeID " + typeID + " not allowed for literals.");
        }
    }

    @Override
    public long getTotalVersionNumber() {
        return getVersion();
    }

    @Override
    public long evaluate() {
        return value;
    }

    @Override
    public String toString() {
        return "" + value;
    }
}

package aoc21.days.day16;

public abstract class Packet {
    private final int version;
    private final int typeID;
    private final long length;

    protected Packet(int version, int typeID, long length) {
        this.version = version;
        this.typeID = typeID;
        this.length = length;
    }

    public long getLength() {
        return length;
    }

    public int getVersion() {
        return version;
    }

    public int getTypeID() {
        return typeID;
    }

    public abstract long getTotalVersionNumber();

    public abstract long evaluate();
}

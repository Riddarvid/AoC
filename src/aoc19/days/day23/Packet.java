package aoc19.days.day23;

public class Packet {
    private long sender;
    private final long address;
    private final long x;
    private final long y;
    private boolean hasReturnedX;
    private boolean hasReturnedY;

    public Packet(long sender, long address, long x, long y) {
        this.sender = sender;
        this.address = address;
        this.x = x;
        this.y = y;
        hasReturnedX = false;
    }

    public Packet(Packet other) {
        this(other.sender, other.address, other.x, other.y);
    }

    public long getAddress() {
        return address;
    }

    public long getSender() {
        return sender;
    }

    public long getNext() {
        if (!hasReturnedX) {
            hasReturnedX = true;
            return x;
        }
        if (!hasReturnedY) {
            hasReturnedY = true;
            return y;
        }
        throw new IllegalStateException("Both components have been accessed");
    }

    public boolean isEmpty() {
        return hasReturnedX && hasReturnedY;
    }

    public long getY() {
        return y;
    }

    public void setSender(long sender) {
        this.sender = sender;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }


}

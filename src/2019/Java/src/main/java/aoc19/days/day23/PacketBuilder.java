package aoc19.days.day23;

public class PacketBuilder {
    private long sender;
    private long address;
    private long x;
    private long y;

    private boolean hasSetAddress;
    private boolean hasSetX;
    private boolean hasSetY;

    public PacketBuilder(long sender) {
        this.sender = sender;
    }


    public void setNext(long value) {
        if (!hasSetAddress) {
            hasSetAddress = true;
            address = value;
        } else if (!hasSetX) {
            hasSetX = true;
            x = value;
        } else if (!hasSetY) {
            hasSetY = true;
            y = value;
        } else {
            throw new IllegalStateException();
        }
    }

    public boolean isDone() {
        return hasSetAddress && hasSetX && hasSetY;
    }

    public Packet build() {
        if (!isDone()) {
            throw new IllegalStateException();
        }
        return new Packet(sender, address, x, y);
    }
}

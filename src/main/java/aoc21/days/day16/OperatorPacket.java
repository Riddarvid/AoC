package aoc21.days.day16;

import java.util.List;

public class OperatorPacket extends Packet {
    private final List<Packet> subPackets;

    protected OperatorPacket(int version, int typeID, long length, List<Packet> subPackets) {
        super(version, typeID, length);
        this.subPackets = subPackets;
        if (typeID == 4) {
            throw new IllegalArgumentException("TypeID " + typeID + " not allowed for operators.");
        }
    }

    @Override
    public long getTotalVersionNumber() {
        long sum = getVersion();
        for (Packet subPacket : subPackets) {
            sum += subPacket.getTotalVersionNumber();
        }
        return sum;
    }

    @Override
    public long evaluate() {
        switch (getTypeID()) {
            case 0: return sum();
            case 1: return product();
            case 2: return minimum();
            case 3: return maximum();
            case 5: return greaterThan();
            case 6: return lessThan();
            case 7: return equalTo();
        }
        throw new IllegalArgumentException("Illegal type ID: " + getTypeID());
    }

    private long sum() {
        long sum = 0;
        for (Packet subPacket : subPackets) {
            sum += subPacket.evaluate();
        }
        return sum;
    }

    private long product() {
        long product = 1;
        for (Packet subPacket : subPackets) {
            product *= subPacket.evaluate();
        }
        return product;
    }

    private long minimum() {
        long min = Long.MAX_VALUE;
        for (Packet subPacket : subPackets) {
            long value = subPacket.evaluate();
            if (value < min) {
                min = value;
            }
        }
        return min;
    }

    private long maximum() {
        long max = 0;
        for (Packet subPacket : subPackets) {
            long value = subPacket.evaluate();
            if (value > max) {
                max = value;
            }
        }
        return max;
    }

    private long greaterThan() {
        if (subPackets.get(0).evaluate() > subPackets.get(1).evaluate()) {
            return 1;
        }
        return 0;
    }

    private long lessThan() {
        if (subPackets.get(0).evaluate() < subPackets.get(1).evaluate()) {
            return 1;
        }
        return 0;
    }

    private long equalTo() {
        if (subPackets.get(0).evaluate() == subPackets.get(1).evaluate()) {
            return 1;
        }
        return 0;
    }
}

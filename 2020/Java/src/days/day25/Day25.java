package days.day25;

import riddarvid.aoc.days.Day;

import java.math.BigInteger;

public class Day25 extends Day {
    private long publicKeyCard;
    private long publicKeyDoor;

    public static void main(String[] args) {
        new Day25().runAndPrint();
    }

    @Override
    public long part1() {
        long loopSize = 0;
        long value = 1;
        long subjectNumber = 7;
        long modulus = 20201227;
        while (value != publicKeyCard && value != publicKeyDoor) {
            value = (value * subjectNumber) % modulus;
            loopSize++;
        }
        long encryptionKey;
        if (value == publicKeyCard) {
            encryptionKey = BigInteger.valueOf(publicKeyDoor).modPow(BigInteger.valueOf(loopSize), BigInteger.valueOf(modulus)).longValue();
        } else {
            encryptionKey = BigInteger.valueOf(publicKeyCard).modPow(BigInteger.valueOf(loopSize), BigInteger.valueOf(modulus)).longValue();
        }
        return encryptionKey;
    }

    @Override
    public long part2() {
        return 0;
    }

    @Override
    public void setup() {
        publicKeyCard = Long.parseLong(lines.get(0));
        publicKeyDoor = Long.parseLong(lines.get(1));
    }
}

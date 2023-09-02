package aoc21.days.day16;

import aoc.days.Day;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class Day16 extends Day {
    private String hexCode;
    private Packet rootPacket;

    @Override
    public void setup() {
        hexCode = lines.get(0);
        String binary = hexToBinaryPadded(hexCode);
        rootPacket = parsePacket(binary);
    }

    private String hexToBinaryPadded(String hexCode) {
        StringBuilder binary = new StringBuilder(new BigInteger('1' + hexCode, 16).toString(2));
        binary.replace(0, 1, "");
        while (binary.length() % 4 != 0) {
            binary.append('0');
        }
        return binary.toString();
    }

    private long binaryToDecimal(String binary) {
        return new BigInteger(binary, 2).longValue();
    }

    private Packet parsePacket(String binary) {
        int version = (int) binaryToDecimal(binary.substring(0, 3));
        int type = (int) binaryToDecimal(binary.substring(3, 6));
        if (type == 4) {
            return parseLiteralPacket(version, type, binary.substring(6));
        } else {
            return parseOperatorPacket(version, type, binary.substring(6));
        }
    }

    private LiteralPacket parseLiteralPacket(int version, int type, String binary) {
        StringBuilder resultString = new StringBuilder();
        int i = 0;
        while (binary.charAt(i) == '1') {
            resultString.append(binary, i + 1, i + 5);
            i += 5;
        }
        resultString.append(binary, i + 1, i + 5);
        long value = binaryToDecimal(resultString.toString());
        long length = 3 + 3 + i + 5;
        return new LiteralPacket(version, type, length, value);
    }

    private OperatorPacket parseOperatorPacket(int version, int type, String binary) {
        int lengthTypeID = binary.charAt(0) - '0';
        if (lengthTypeID == 0) {
            long length = binaryToDecimal(binary.substring(1, 16));
            return parseOperatorPacketByLength(version, type, length, binary.substring(16));
        } else {
            long numberOfSubPackets = binaryToDecimal(binary.substring(1, 12));
            return parseOperatorPacketByNumberOfSubPackets(version, type, numberOfSubPackets, binary.substring(12));
        }
    }

    private OperatorPacket parseOperatorPacketByLength(int version, int type, long length, String binary) {
        int i = 0;
        List<Packet> subPackets = new ArrayList<>();
        while (i < length) {
            Packet subPacket = parsePacket(binary.substring(i));
            subPackets.add(subPacket);
            i += subPacket.getLength();
        }
        long packetLength = 3 + 3 + 1 + 15 + length;
        return new OperatorPacket(version, type, packetLength, subPackets);
    }

    private OperatorPacket parseOperatorPacketByNumberOfSubPackets(int version, int type, long numberOfSubPackets, String binary) {
        List<Packet> subPackets = new ArrayList<>();
        int index = 0;
        for (int i = 0; i < numberOfSubPackets; i++) {
            Packet subPacket = parsePacket(binary.substring(index));
            subPackets.add(subPacket);
            index += subPacket.getLength();
        }
        long packetLength = 3 + 3 + 1 + 11 + index;
        return new OperatorPacket(version, type, packetLength, subPackets);
    }

    @Override
    public long part1() {
        return rootPacket.getTotalVersionNumber();
    }



    @Override
    public long part2() {
        return rootPacket.evaluate();
    }
}

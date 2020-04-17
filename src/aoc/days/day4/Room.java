package aoc.days.day4;

import aoc.utils.encryption.EncryptionUtils;
import aoc.utils.input.InputUtils;

import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Room {
    private final String encryptedName;
    private final int sectorID;
    private final String checksum;
    private String decryptedName;

    public Room(String input) {
        List<String> tokens = InputUtils.getTokens(input, "[");
        String checksum = tokens.get(1);
        checksum = checksum.replace("]", "");
        this.checksum = checksum;
        String nameAndID = tokens.get(0);
        int i = nameAndID.length() - 1;
        while (nameAndID.charAt(i) != '-') {
            i--;
        }
        encryptedName = nameAndID.substring(0, i);
        sectorID = Integer.parseInt(nameAndID.substring(i + 1));
    }

    public String getEncryptedName() {
        return encryptedName;
    }

    public int getSectorID() {
        return sectorID;
    }

    public String getChecksum() {
        return checksum;
    }

    public boolean isValid() {
        Queue<Character> pq = new PriorityQueue<>(new RoomComparator(encryptedName));
        for (char c = 'a'; c <= 'z'; c++) {
            pq.add(c);
        }
        for (char c : checksum.toCharArray()) {
            if (c != pq.poll()) {
                return false;
            }
        }
        return true;
    }

    public void decrypt() {
        decryptedName = EncryptionUtils.caesarDecrypt(encryptedName, sectorID % 26);
    }

    public String getDecryptedName() {
        return decryptedName;
    }
}

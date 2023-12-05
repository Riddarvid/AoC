package aoc15.days.day4;

import aoc.days.Day;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Day4 extends Day {
    private String key;

    @Override
    public long part1() {
        MessageDigest digest = null;
        try {
            digest = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
        int i = 0;
        byte[] hash;
        do {
            i++;
            String message = key + i;
            hash = digest.digest(message.getBytes());
        } while (hash[0] != 0 || hash[1] != 0 || ((hash[2] + 128) % 128) > 15);
        return i;
    }

    @Override
    public long part2() {
        MessageDigest digest = null;
        try {
            digest = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
        int i = 0;
        byte[] hash;
        do {
            i++;
            String message = key + i;
            hash = digest.digest(message.getBytes());
        } while (hash[0] != 0 || hash[1] != 0 || hash[2] != 0);
        return i;
    }

    @Override
    public void setup() {
        key = lines.get(0);
    }
}

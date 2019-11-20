package AoC.day4;

import AoC.Day;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

public class Day4 extends Day {
    private String key;

    public static void main(String[] args) {
        new Day4();
    }

    @Override
    protected void part1() {
        try {
            MessageDigest digest = MessageDigest.getInstance("MD5");
            int i = 0;
            byte[] hash;
            do {
                i++;
                String message = key + i;
                hash = digest.digest(message.getBytes());
            } while (hash[0] != 0 || hash[1] != 0 || ((hash[2] + 128) % 128) > 15);
            System.out.println(i);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void part2() {
        try {
            MessageDigest digest = MessageDigest.getInstance("MD5");
            int i = 0;
            byte[] hash;
            do {
                i++;
                String message = key + i;
                hash = digest.digest(message.getBytes());
            } while (hash[0] != 0 || hash[1] != 0 || hash[2] != 0);
            System.out.println(i);
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void setup() {
        key = lines.get(0);
    }
}

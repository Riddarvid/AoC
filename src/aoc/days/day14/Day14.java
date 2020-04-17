package aoc.days.day14;

import aoc.days.Day;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.InputMismatchException;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day14 extends Day {
    private String salt;

    public static void main(String[] args) {
        new Day14();
    }

    @Override
    protected void part1() {
        List<Integer> keys = new ArrayList<>();
        List<String> hashes = generateHashes(salt, 30000, 1);
        int i = 0;
        while (keys.size() < 64) {
            if (isKey(i, hashes)) {
                keys.add(i);
            }
            i++;
        }
        System.out.println(keys.get(keys.size() - 1));
    }

    private boolean isKey(int index, List<String> hashes) {
        if (hasTriplet(hashes.get(index))) {
            char c = getTripletChar(hashes.get(index));
            for (int i = index + 1; i <= index + 1000; i++) {
                if (hasFive(hashes.get(i), c)) {
                    return true;
                }
            }
        }
        return false;
    }

    private List<String> generateHashes(String salt, int number, int repeats) {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
        List<String> hashes = new ArrayList<>();
        for (int index = 0; index < number; index++) {
            String s = salt + index;
            for (int i = 0; i < repeats; i++) {
                byte[] hash = md.digest(s.getBytes());
                s = bytesToHex(hash);
            }
            hashes.add(s);
        }
        return hashes;
    }

    private boolean hasFive(String hash, char c) {
        int i = 0;
        while (i < hash.length() - 4) {
            if (c == hash.charAt(i) && c == hash.charAt(i + 1) && c == hash.charAt(i + 2) && c == hash.charAt(i + 3) && c == hash.charAt(i + 4)) {
                return true;
            }
            i++;
        }
        return false;
    }

    private char getTripletChar(String hash) {
        int i = 0;
        while (i < hash.length() - 2) {
            char c = hash.charAt(i);
            if (c == hash.charAt(i + 1) && c == hash.charAt(i + 2)) {
                return c;
            }
            i++;
        }
        throw new InputMismatchException();
    }

    private boolean hasTriplet(String hash) {
        /*Pattern p = Pattern.compile("(\\w)\\1{2}");
        Matcher m = p.matcher(hash);
        return m.find();*/
        int i = 0;
        while (i < hash.length() - 2) {
            char c = hash.charAt(i);
            if (c == hash.charAt(i + 1) && c == hash.charAt(i + 2)) {
                return true;
            }
            i++;
        }
        return false;
    }

    private String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    @Override
    protected void part2() {
        List<Integer> keys = new ArrayList<>();
        List<String> hashes = generateHashes(salt, 25000, 2017);
        int i = 0;
        while (keys.size() < 64) {
            if (isKey(i, hashes)) {
                keys.add(i);
            }
            i++;
        }
        System.out.println(keys.get(keys.size() - 1));
    }

    @Override
    protected void setup() {
        salt = lines.get(0);
    }
}

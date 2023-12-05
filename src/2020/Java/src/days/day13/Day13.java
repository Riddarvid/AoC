package days.day13;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Day13 extends Day {
    private int earliest;
    private List<Integer> buses;
    private List<CrtTerm> crtTerms;

    public static void main(String[] args) {
        new Day13().runAndPrint();
    }

    @Override
    public long part1() {
        int departTime = earliest;
        while (!busDeparts(buses, departTime)) {
            departTime++;
        }
        int departingBus = getDepartingBus(buses, departTime);
        int wait = departTime - earliest;
        return departingBus * wait;
    }

    private int getDepartingBus(List<Integer> buses, int departTime) {
        for (int bus : buses) {
            if (departTime % bus == 0) {
                return bus;
            }
        }
        throw new InputMismatchException("No departing bus");
    }

    private boolean busDeparts(List<Integer> buses, int departTime) {
        for (int bus : buses) {
            if (departTime % bus == 0) {
                return true;
            }
        }
        return false;
    }

    @Override
    public long part2() {
        BigInteger t = crt(crtTerms);
        return t.longValue();
    }

    private BigInteger crt(List<CrtTerm> crtTerms) {
        BigInteger sum = BigInteger.ZERO;
        BigInteger N = BigInteger.ONE;
        for (CrtTerm crtTerm : crtTerms) {
            N = N.multiply(crtTerm.getModulus());
        }
        for (CrtTerm crtTerm : crtTerms) {
            sum = sum.add(getTerm(N, crtTerm));
        }
        return sum.mod(N);
    }

    private BigInteger getTerm(BigInteger N, CrtTerm crtTerm) {
        BigInteger Ni = N.divide(crtTerm.getModulus());
        return crtTerm.getRemainder().multiply(Ni).multiply(Ni.modInverse(crtTerm.getModulus()));
    }

    @Override
    public void setup() {
        earliest = Integer.parseInt(lines.get(0));
        buses = ParsingUtils.getIntegers(lines.get(1));
        crtTerms = new ArrayList<>();
        List<String> tokens = ParsingUtils.getTokens(lines.get(1), ',');
        for (int i = 0; i < tokens.size(); i++) {
            String s = tokens.get(i);
            if (!s.equals("x")) {
                int modulus = Integer.parseInt(s);
                int remainder = (modulus - i) % modulus;
                crtTerms.add(new CrtTerm(remainder, modulus));
            }
        }
    }
}

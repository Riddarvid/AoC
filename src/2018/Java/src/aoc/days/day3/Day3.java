package aoc.days.day3;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day3 extends Day {
    List<Claim> claims;
    int[][] numberOfClaims;

    public static void main(String[] args) {
        new Day3();
    }

    @Override
    protected void part1() {
        numberOfClaims = new int[1000][1000];
        for (Claim claim : claims) {
            markClaim(numberOfClaims, claim);
        }
        System.out.println(getOverlaps(numberOfClaims));
    }

    private int getOverlaps(int[][] numberOfClaims) {
        int overlaps = 0;
        for (int row = 0; row < numberOfClaims.length; row++) {
            for (int col = 0; col < numberOfClaims[0].length; col++) {
                if (numberOfClaims[row][col] > 1) {
                    overlaps++;
                }
            }
        }
        return overlaps;
    }

    private void markClaim(int[][] numberOfClaims, Claim claim) {
        for (int row = claim.getOffsetY(); row < claim.getOffsetY() + claim.getHeight(); row++) {
            for (int col = claim.getOffsetX(); col < claim.getOffsetX() + claim.getWidth(); col++) {
                numberOfClaims[row][col]++;
            }
        }
    }

    @Override
    protected void part2() {
        for (Claim claim : claims) {
            if (overlaps(claim, numberOfClaims)) {
                System.out.println(claim.getId());
                break;
            }
        }
    }

    private boolean overlaps(Claim claim, int[][] numberOfClaims) {
        for (int row = claim.getOffsetY(); row < claim.getOffsetY() + claim.getHeight(); row++) {
            for (int col = claim.getOffsetX(); col < claim.getOffsetX() + claim.getWidth(); col++) {
                if (numberOfClaims[row][col] != 1) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    protected void setup() {
        claims = new ArrayList<>();
        for (String string : lines) {
            claims.add(new Claim(ParsingUtils.getIntegers(string)));
        }
    }
}

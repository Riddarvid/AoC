package aoc.days.day23;

public class Day23alt {
    public static void main(String[] args) {
        int c = 126300;
        int h = 0;
        for (int b = 109300; b <= c; b += 17) {
            boolean shouldInc = false;
            for (int d = 2; d < b; d++) {
                for (int e = 2; e < b; e++) {
                    if (e * d == b) {
                        shouldInc = true;
                        break;
                    }
                }
                if (shouldInc) {
                    break;
                }
            }
            if (shouldInc) {
                h++;
            }
        }
        System.out.println(h);
    }
}

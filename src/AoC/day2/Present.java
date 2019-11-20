package AoC.day2;

import java.util.List;

public class Present {
    int s1;
    int s2;
    int s3;
    int a1;
    int a2;
    int a3;
    int surfaceArea;
    int paperRequired;
    int ribbonRequired;

    public Present(List<Integer> dimensions) {
        s1 = dimensions.get(0);
        s2 = dimensions.get(1);
        s3 = dimensions.get(2);
        int temp;
        if (s2 < s1) {
            temp = s2;
            s2 = s1;
            s1 = temp;
        }
        if (s3 < s1) {
            temp = s3;
            s3 = s1;
            s1 = temp;
        }
        if (s3 < s2) {
            temp = s3;
            s3 = s2;
            s2 = temp;
        }
        a1 = s1 * s2;
        a2 = s2 * s3;
        a3 = s3 * s1;
        surfaceArea = 2 * (a1 + a2 + a3);
        paperRequired = surfaceArea + a1;
        ribbonRequired = 2 * (s1 + s2) + s1 * s2 * s3;
    }
}

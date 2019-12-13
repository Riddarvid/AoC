package aoc.days.day3;

import aoc.utils.math.Point;

import java.util.ArrayList;
import java.util.List;

public class Line {
    private List<Segment> segments;

    public Line() {
        segments = new ArrayList<>();
    }

    public void addSegment(Segment segment) {
        segments.add(segment);
    }

    public List<Intersection> getIntersections(Line other) {
        List<Intersection> intersections = new ArrayList<>();
        for (int i = 0; i < other.segments.size(); i++) {
            Segment os = other.segments.get(i);
            for (int j = 0; j < segments.size(); j++) {
                Segment s = segments.get(j);
                Point intersection = s.getIntersection(os);
                if (intersection != null) {
                    int length1 = length(j - 1, segments) + s.distance(intersection);
                    int length2 = length(i - 1, other.segments) + os.distance(intersection);
                    intersections.add(new Intersection(intersection, length1 + length2));
                }
            }
        }
        return intersections;
    }

    private int length(int segmentIndex, List<Segment> segments) {
        int sum = 0;
        for (int i = 0; i <= segmentIndex; i++) {
            sum += segments.get(i).length();
        }
        return sum;
    }
}

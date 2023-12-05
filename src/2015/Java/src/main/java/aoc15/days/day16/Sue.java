package aoc15.days.day16;

import java.util.ArrayList;
import java.util.List;

public class Sue {
    private final int id;
    private List<Sample> samples = new ArrayList<>();

    public Sue(int id) {
        this.id = id;
    }

    public Sue(List<Sample> samples) {
        id = -1;
        this.samples = new ArrayList<>(samples);
    }

    public List<Sample> getSamples() {
        return samples;
    }

    public void addSample(Sample sample) {
        samples.add(sample);
    }

    public boolean isPotential(Sue other) {
        for (Sample sample : other.samples) {
            if (!samples.contains(sample)) {
                return false;
            } else if (sample.getCount() != samples.get(samples.indexOf(sample)).getCount()) {
                return false;
            }
        }
        return true;
    }

    public int getId() {
        return id;
    }

    public boolean isPotential2(Sue other) {
        for (Sample sample : other.samples) {
            if (samples.contains(sample)) {
                Sample thisSample = samples.get(samples.indexOf(sample));
                if (sample.getCompound() == Compound.CATS || sample.getCompound() == Compound.TREES) {
                    if (sample.getCount() <= thisSample.getCount()) {
                        return false;
                    }
                } else if (sample.getCompound() == Compound.POMERANIANS || sample.getCompound() == Compound.GOLDFISH) {
                    if (sample.getCount() >= thisSample.getCount()) {
                        return false;
                    }
                } else {
                    if (sample.getCount() != thisSample.getCount()) {
                        return false;
                    }
                }
            } else {
                return false;
            }
        }
        return true;
    }
}

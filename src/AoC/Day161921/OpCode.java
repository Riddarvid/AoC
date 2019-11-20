package AoC.Day161921;

import AoC.Day161921.OpCodes.OpcodeFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class OpCode {
    private int code;
    private List<OpcodeFunction> behaviours = new ArrayList<>();

    public OpCode(int code) {
        this.code = code;
    }

    public OpCode(OpcodeFunction function) {
        this.behaviours.add(function);
    }

    public void addBehaviour(OpcodeFunction f) {
        if (!behaviours.contains(f)) {
            behaviours.add(f);
        }
    }

    public List<OpcodeFunction> getBehaviours() {
        return behaviours;
    }

    @Override
    public String toString() {
        return code + " " + behaviours.get(0).toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OpCode opCode = (OpCode) o;
        return Objects.equals(behaviours, opCode.behaviours);
    }

    @Override
    public int hashCode() {
        return Objects.hash(behaviours);
    }

    public int getCode() {
        return code;
    }
}

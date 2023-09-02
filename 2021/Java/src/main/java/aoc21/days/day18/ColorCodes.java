package aoc21.days.day18;

import java.util.HashMap;
import java.util.Map;

public class ColorCodes {
    public final static Map<Integer, String> colorCodeMap = new HashMap<>();
    public final static String RESET = "\u001B[0m";

    static {
        colorCodeMap.put(0, "\u001B[38;2;31;164;86m");//Cyan
        colorCodeMap.put(1, "\u001B[38;2;59;214;44m");//Green
        colorCodeMap.put(2, "\u001B[38;2;252;239;2m");//Yellow
        colorCodeMap.put(3, "\u001B[38;2;252;144;2m");//Orange
        colorCodeMap.put(4, "\u001B[38;2;252;5;5m");//Red
    }
}

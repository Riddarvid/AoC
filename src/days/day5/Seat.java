package days.day5;

public class Seat {
    private final int row;
    private final int column;

    public Seat(String spacing) {
        //B:1 F:0
        //R:1 L:0
        int row = 0;
        for (int i = 0; i < 7; i++) {
            row *= 2;
            if (spacing.charAt(i) == 'B') {
                row++;
            }
        }
        this.row = row;
        int column = 0;
        for (int i = 7; i < 10; i++) {
            column *= 2;
            if (spacing.charAt(i) == 'R') {
                column++;
            }
        }
        this.column = column;
    }

    public int getSeatID() {
        return row * 8 + column;
    }
}

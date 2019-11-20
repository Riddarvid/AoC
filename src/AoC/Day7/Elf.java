package AoC.Day7;

public class Elf {
    private Integer currentTask;
    private int progress;

    public void assignTask(Integer task) {
        currentTask = task;
        progress = 60 + task + 1;
    }

    public boolean hasTask() {
        return currentTask != null;
    }

    public Integer work() {
        progress--;
        if (progress == 0) {
            Integer completed = currentTask;
            currentTask = null;
            return completed;
        }
        return null;
    }

    public int getProgress() {
        return progress;
    }

    public Integer getCurrentTask() {
        return currentTask;
    }
}
